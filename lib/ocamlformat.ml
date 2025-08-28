open Utils.Misc

module Error = struct
  type syntax = {
    line : int;
    start_col: int;
    end_col: int;
    message : string list;
  }

  type internal = string

  type t =
    | Syntax of syntax
    | Internal of internal

  let to_string = function
    | Syntax {line; start_col; end_col; message} ->
      Printf.sprintf "syntax error: line %d.%d-%d: %s"
        line start_col end_col (String.concat "\\n" message)
    | Internal message ->
      Printf.sprintf "internal error: %s" message
end

module Output_parser = struct

  (* Error message shape 1:

     File "%s", line %d, characters %d-%d:
     Error: %s
  *)

  let error_message_1 text ic =
    Scanf.sscanf_opt text
      {|File %S, line %d, characters %d-%d:|}
      (fun input line start_col end_col ->
         let rec message acc =
           let line = input_line ic in
           if String.starts_with ~prefix:"Error: " line ||
              (String.starts_with ~prefix:"  " line && acc = [])
           then List.rev (line :: acc)
           else message (line :: acc)
         in
         let message = message [] in
         (input, Error.Syntax {line; start_col; end_col; message})
      )

  (* Error message shape 2:

     ocamlformat: ignoring "%s" (syntax error)
  *)

  let error_message_2 text =
    Scanf.sscanf_opt text
      {|ocamlformat: ignoring %S (syntax error)|}
      (fun path -> path)

  (* Error message shape 3:

     ocamlformat: Cannot process %S.
       Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.

     This preceeds one or more message of the form
       BUG: %s
     where bug apply
  *)

  let error_message_3_part_1 text ic =
    Scanf.sscanf_opt text
      {|ocamlformat: Cannot process %S.|}
      (fun input ->
         match input_line ic with
         | "  Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues." ->
           input
         | line -> failwithf "driver: %s: unexpected error header: %s" input line
      )

  let error_message_3_part_2 text =
    let bug = "  BUG: " in
    if String.starts_with ~prefix:bug text then
      let bugl = String.length bug in
      let txtl = String.length text in
      let message = String.sub text bugl (txtl - bugl) in
      Some (Error.Internal message)
    else
      None

  let rec next_error buggy_input ic =
    match input_line ic with
    | exception End_of_file -> None
    | "" -> next_error buggy_input ic
    | line ->
      let next =
        try
          match error_message_1 line ic with
          | Some _ as r -> r
          | None ->
            match error_message_3_part_1 line ic with
            | Some input ->
              buggy_input := input;
              None
            | None ->
              match error_message_3_part_2 line with
              | Some err -> Some (!buggy_input, err)
              | None ->
                match error_message_2 line with
                | Some _path ->
                  (* This message is just noise preceeding a real syntax error message *)
                  None
                | None ->
                  failwithf "driver: error: unexpected output: %s" line
        with End_of_file ->
          failwithf "driver: error: unexpected end of file after line: %s" line
      in
      match next with
      | Some _ as r -> r
      | None -> next_error buggy_input ic

  let read_errors ic =
    let buggy_input = ref "" in
    let rec loop acc =
      match next_error buggy_input ic with
      | None -> List.rev acc
      | Some x -> loop (x :: acc)
    in
    loop []

  let rev_cleanup_errors answers =
    let rec loop acc = function
      | (input, Error.Internal "comment changed.") :: rest -> (
          match rest with
          | (input', Error.Syntax se) :: _ when
              input = input' &&
              String.starts_with ~prefix:"Error: comment (*"
                (List.hd se.message)
            ->
            loop acc rest
          | _ ->
            let msg = match rest with
              | [] -> "end of file"
              | (file, err) :: _ -> file ^ ": " ^ Error.to_string err
            in
            failwithf "driver: error: expecting error details after \
                       \"comment changed.\", got %s" msg
        )
      | x :: xs -> loop (x :: acc) xs
      | [] -> acc
    in
    loop [] answers
end

let check_command = ["--check"; "--enable-outside-detected-project"]

let batch_size = 80

let temp_dir = Filename.get_temp_dir_name ()

let temp_path id i ext =
  Printf.sprintf "%s/ocamlgrammarfuzzer_%d-%d.%s" temp_dir id i ext

let temp_path_index name =
  let name = Filename.remove_extension (Filename.basename name) in
  Scanf.sscanf_opt name "ocamlgrammarfuzzer_%d-%d" (fun _id index -> index)

let environ = lazy (Unix.environment ())

let batch_ids = ref 0

let start_batch ~ocamlformat_command = function
  | [] -> None
  | inputs ->
    let id = !batch_ids in
    incr batch_ids;
    let files = List.mapi (fun i (kind, source) ->
        let ext = match kind with `Intf -> "mli" | `Impl -> "ml" in
        let path = temp_path id i ext in
        let oc = open_out_bin path in
        output_string oc source;
        output_char oc '\n';
        close_out oc;
        path
      ) inputs
    in
    let process =
      Unix.open_process_args_full
        ocamlformat_command
        (Array.of_list (ocamlformat_command :: check_command @ files))
        (Lazy.force environ)
    in
    Some (files, process)

let consume_batch = function
  | None -> Seq.empty
  | Some (files, (_, _, pstderr as process)) ->
    let errors =
      let finally () =
        List.iter (fun x -> try Unix.unlink x with _ -> ()) files;
        ignore (Unix.close_process_full process);
      in
      match Output_parser.read_errors pstderr with
      | r -> finally (); r
      | exception exn -> finally (); raise exn
    in
    let errors = Output_parser.rev_cleanup_errors errors in
    let answer = Array.make (List.length files) [] in
    List.iter begin fun (input, error) ->
      match temp_path_index input with
      | Some index -> answer.(index) <- error :: answer.(index)
      | None -> failwithf "driver: error: unexpected filename %S, \
                           expecting ocamlgrammarfuzzer_%%06d.{ml,mli}" input
    end errors;
    Array.to_seq answer

type 'a pure_queue = {
  head: 'a list;
  tail: 'a list;
}

let empty = {head = []; tail = []}

let push xs x = {xs with tail = x :: xs.tail}

let pop = function
  | {head = x :: xs; tail} -> Some (x, {head = xs; tail})
  | {head = []; tail} ->
    match List.rev tail with
    | [] -> None
    | x :: xs -> Some (x, {head = xs; tail = []})

(* Poor man's work queue: force the sequence [jobs] item ahead *)
let overlapping_force jobs seq =
  let rec initialize queue seq = function
    | 0 -> queue, seq
    | n ->
      match seq () with
      | Seq.Nil -> queue, Seq.empty
      | Seq.Cons (x, seq') -> initialize (push queue x) seq' (n - 1)
  in
  let queue, seq = initialize empty seq jobs in
  let rec reconstruct queue seq () =
    match pop queue with
    | None -> seq ()
    | Some (x, queue') ->
      match seq () with
      | Seq.Nil -> Seq.Cons (x, reconstruct queue' Seq.empty)
      | Seq.Cons (x', seq') ->
        Seq.Cons (x, reconstruct (push queue' x') seq')
  in
  reconstruct queue seq

let check ?(ocamlformat_command="ocamlformat") ?(jobs=0) seq =
  seq
  |> (* Group by batches of appropriate size *)
  batch_by ~size:batch_size
  |> (* Launch a process for each batch *)
  Seq.map (start_batch ~ocamlformat_command)
  |> (* Force sequence enough items ahead to kick [jobs] processes ahead *)
  overlapping_force jobs
  |> (* Collect the results *)
  Seq.concat_map consume_batch
