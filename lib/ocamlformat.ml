open Utils.Misc

type location = {
    line : int;
    start_col: int;
    end_col: int;
}

type error = {
  message: string;
  location: location option;
}

let error ?location message = { message; location}

let error_to_string {message; location} =
  match location with
  | None ->
    Printf.sprintf "error: %s" message
  | Some {line; start_col; end_col} ->
    Printf.sprintf "error: line %d.%d-%d: %s"
      line start_col end_col message

let input_line ?(debug=ignore) ic =
  let result = input_line ic in
  debug result;
  result

module Output_parser = struct

  (* Error message shape 1:

     Starts with a line:
     > File "%s", line %d, characters %d-%d:

     Followed by an optional quotation of problematic source:
     > %d | ... problem ...
                ^^^^^^^

     Terminated by:
     > Error: %s
     or:
     >   This %s is unmatched.

     The special case:
     > Error: comment (*  C%d  *) dropped.
     is also detected here.

     Once the first line has been identified, parsing should fail if the
     following lines do not match the template.
  *)

  (* Replace error messages mentioning a specific comment with a generic text *)
  let comment_dropped = "Error: comment dropped."

  let error_message_1 text ic =
    Scanf.sscanf_opt text
      {|File %S, line %d, characters %d-%d:|}
      (fun input line start_col end_col ->
         let match_terminator text =
           match Scanf.sscanf_opt text "%d | %s" (fun _ _ -> ()) with
           | Some _ -> None
           | None ->
             let location = {line; start_col; end_col} in
             match Scanf.sscanf_opt text "Error: comment (*  C%d  *) dropped." (fun _ -> ()) with
             | Some () -> Some (Some (input, error ~location comment_dropped))
             | None ->
               if String.starts_with ~prefix:"Error: " text then
                 Some (Some (input, error ~location text))
               else if String.starts_with ~prefix:"  This "  text then
                 Some None
               else
                 failwithf "Unexpected line %S" text
         in
         match match_terminator (ic ()) with
         | Some result -> result
         | _ ->
           let caret = ref false in
           let pred = function
             | ' ' -> not !caret
             | '^' -> caret := true; true
             | _ -> false
           in
           while not (String.for_all pred (ic ())) do
             caret := false
           done;
           let text = ic () in
           match match_terminator text with
           | None -> failwithf "Unexpected line %S (looking for error terminator)" text
           | Some result -> result
      )

  (* Error message shape 2:

     ocamlformat: ignoring "%s" (syntax error)
  *)

  let error_message_2 text =
    Scanf.sscanf_opt text
      {|%s@: ignoring %S (syntax error)|}
      (fun _ocamlformat path -> path)

  (* Error message shape 3:

     ocamlformat: Cannot process %S.
       Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.

     This preceeds one or more message of the form
       BUG: %s
     where bug apply
  *)

  let error_message_3_part_1 text ic =
    Scanf.sscanf_opt text
      {|%s@: Cannot process %S.|}
      (fun _ocamlformat input ->
         match ic () with
         | "  Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues." ->
           input
         | line -> failwithf "driver: %s: unexpected error header: %s" input line
      )

  let error_message_3_part_2 text ic =
    if text = "  BUG: unhandled exception." then
      Some (error ("Exception: " ^ ic ()))
    else if String.starts_with ~prefix:"  BUG: " text then
      Some (error (String.trim text))
    else
      None

  let rec next_error buggy_input ic =
    match ic () with
    | exception End_of_file -> None
    | "" -> next_error buggy_input ic
    | line ->
      let next =
        try
          match error_message_1 line ic with
          | Some r -> r
          | None ->
            match error_message_3_part_1 line ic with
            | Some input ->
              buggy_input := input;
              None
            | None ->
              match error_message_3_part_2 line ic with
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
      | (input, {message = "BUG: comment changed."; location = None}) :: rest ->
        begin match rest with
          | (input', error) :: _ when input = input' && error.message = comment_dropped ->
            loop acc rest
          | _ ->
            let msg = match rest with
              | [] -> "end of file"
              | (file, err) :: _ -> file ^ ": " ^ error_to_string err
            in
            failwithf "driver: error: expecting error details after \
                       \"comment changed.\", got %s" msg
        end
      | x :: xs -> loop (x :: acc) xs
      | [] -> acc
    in
    loop [] answers
end

let default_batch_size = 80

let temp_dir = Filename.get_temp_dir_name ()

let temp_path id i ext =
  Filename.concat temp_dir (Printf.sprintf "ocamlgrammarfuzzer_%d-%d.%s" id i ext)

let temp_path_index name =
  let name = Filename.remove_extension (Filename.basename name) in
  Scanf.sscanf_opt name "ocamlgrammarfuzzer_%d-%d" (fun _id index -> index)

let environ = lazy (Unix.environment ())

let batch_ids = ref 0

type source_kind =
  | Impl
  | Intf

let start_batch ?debug_line ~ocamlformat_command ~args = function
  | [] -> None
  | inputs ->
    let id = !batch_ids in
    incr batch_ids;
    let files = List.mapi (fun i (kind, source) ->
        let ext = match kind with Intf -> "mli" | Impl -> "ml" in
        let path = temp_path id i ext in
        let oc = open_out_bin path in
        output_string oc source;
        output_char oc '\n';
        close_out oc;
        begin match debug_line with
          | None -> ()
          | Some f -> Printf.ksprintf f "<%s: %s\n" path source;
        end;
        path
      ) inputs
    in
    let process =
      Unix.open_process_args_full
        ocamlformat_command
        (Array.of_list (ocamlformat_command :: args @ files))
        (Lazy.force environ)
    in
    Some (files, process)

let unlink_no_err path =
  try Unix.unlink path
  with _ -> ()

let consume_batch ?debug_line ~consume ~pack = function
  | None -> Seq.empty
  | Some (files, (_, _, pstderr as process)) ->
    let line_reader () = input_line ?debug:debug_line pstderr in
    let errors = Output_parser.read_errors line_reader in
    ignore (Unix.close_process_full process);
    let files = Array.of_list (List.map consume files) in
    let errors = Output_parser.rev_cleanup_errors errors in
    let answer = Array.make (Array.length files) [] in
    List.iter begin fun (input, error) ->
      match temp_path_index input with
      | Some index -> answer.(index) <- error :: answer.(index)
      | None -> failwithf "driver: error: unexpected filename %S, \
                           expecting ocamlgrammarfuzzer_%%06d.{ml,mli}" input
    end errors;
    Array.to_seq (Array.map2 pack files answer)

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

let check
    ?(ocamlformat_command="ocamlformat")
    ?(jobs=0) ?(batch_size=default_batch_size)
    ?debug_line
    seq
  =
  seq
  |> (* Group by batches of appropriate size *)
  batch_by ~size:batch_size
  |> (* Launch a process for each batch *)
  Seq.map (start_batch ?debug_line ~ocamlformat_command
             ~args:["--check"; "--enable-outside-detected-project"])
  |> (* Force sequence enough items ahead to kick [jobs] processes ahead *)
  overlapping_force jobs
  |> (* Collect the results *)
  Seq.concat_map (consume_batch ?debug_line
                    ~consume:unlink_no_err
                    ~pack:(fun () errors -> errors))

let format
    ?(ocamlformat_command="ocamlformat")
    ?(jobs=0) ?(batch_size=default_batch_size)
    ?debug_line
    seq
  =
  let read_and_unlink path =
    let ic = open_in_bin path in
    let length = in_channel_length ic in
    let contents = really_input_string ic length in
    close_in_noerr ic;
    unlink_no_err path;
    contents
  in
  seq
  |> (* Group by batches of appropriate size *)
  batch_by ~size:batch_size
  |> (* Launch a process for each batch *)
  Seq.map (start_batch ?debug_line ~ocamlformat_command
             ~args:["--inplace"; "--enable-outside-detected-project"])
  |> (* Force sequence enough items ahead to kick [jobs] processes ahead *)
  overlapping_force jobs
  |> (* Collect the results *)
  Seq.concat_map (consume_batch ?debug_line
                    ~consume:read_and_unlink
                    ~pack:(fun contents errors -> (contents, errors)))
