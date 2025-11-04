(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

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

module Line_reader : sig
  type t
  val make : ?hook:(string -> unit) -> in_channel -> t
  val peek : t -> string option
  (* val pop : t -> string option pop*)
  val next : t -> unit
  val peek_exn : t -> string
  val pop_exn : t -> string
end = struct

  type t = {
    ic: in_channel;
    hook: string -> unit;
    mutable line: string option;
  }

  let next t =
    let line = match input_line t.ic with
      | exception End_of_file -> None
      | text ->
        t.hook text;
        Some text
    in
    t.line <- line

  let make ?(hook=ignore) ic =
    let t = {ic; hook; line = None} in
    next t;
    t

  let peek t = t.line

  let pop t =
    let result = t.line in
    next t;
    result

  let get = function
    | None -> raise End_of_file
    | Some text -> text
      
  let peek_exn t = get t.line

  let pop_exn t = get (pop t)
end

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

  let error_message_1 lr =
    Scanf.sscanf_opt (Line_reader.peek_exn lr)
      {|File %S, line %d, characters %d-%d:|}
      (fun input line start_col end_col ->
         Line_reader.next lr;
         let match_terminator text =
           match Scanf.sscanf_opt text "%d | %s" (fun _ _ -> ()) with
           | Some _ -> None
           | None ->
             let location = {line; start_col; end_col} in
             match Scanf.sscanf_opt text "Error: comment (*  C%d  *) dropped." (fun _ -> ()) with
             | Some () -> Some (Some (input, error ~location comment_dropped))
             | None ->
               if String.starts_with ~prefix:"Error: " text then (
                 let rec pump acc =
                   match Line_reader.peek lr with
                   | Some next when StringLabels.starts_with ~prefix:"  " next ->
                     Line_reader.next lr;
                     pump (next :: acc)
                   | _ -> String.concat "\n" (List.rev acc)
                 in
                 Some (Some (input, error ~location (pump [text])))
               ) else if String.starts_with ~prefix:"  This "  text then
                 Some None
               else
                 failwithf "Unexpected line %S" text
         in
         match match_terminator (Line_reader.pop_exn lr) with
         | Some result -> result
         | _ ->
           let caret = ref false in
           let pred = function
             | ' ' -> not !caret
             | '^' -> caret := true; true
             | _ -> false
           in
           while not (String.for_all pred (Line_reader.pop_exn lr)) do
             caret := false
           done;
           let text = Line_reader.pop_exn lr in
           match match_terminator text with
           | None -> failwithf "Unexpected line %S (looking for error terminator)" text
           | Some result -> result
      )

  (* Error message shape 2:

     ocamlformat: ignoring "%s" (syntax error)
  *)

  let error_message_2 lr =
    Scanf.sscanf_opt (Line_reader.peek_exn lr)
      {|%s@: ignoring %S (syntax error)|}
      (fun _ocamlformat path -> path)

  (* Error message shape 3:

     ocamlformat: Cannot process %S.
       Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.

     This preceeds one or more message of the form
       BUG: %s
     where bug apply
  *)

  let error_message_3_part_1 lr =
    Scanf.sscanf_opt (Line_reader.peek_exn lr)
      {|%s@: Cannot process %S.|}
      (fun _ocamlformat input ->
         Line_reader.next lr;
         match Line_reader.pop_exn lr with
         | "  Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues." ->
           input
         | line -> failwithf "driver: %s: unexpected error header: %S" input line
      )

  let error_message_3_part_2 lr =
    let text = Line_reader.peek_exn lr in
    if text = "  BUG: unhandled exception." then (
      Line_reader.next lr;
      Some (error ("Exception: " ^ Line_reader.pop_exn lr))
    ) else if String.starts_with ~prefix:"  BUG: " text then (
      Line_reader.next lr;
      Some (error (String.trim text))
    ) else
      None

  let rec next_error buggy_input lr =
    match Line_reader.peek lr with
    | None -> None
    | Some "" ->
      Line_reader.next lr;
      next_error buggy_input lr
    | Some line ->
      let next =
        try
          match error_message_1 lr with
          | Some r -> r
          | None ->
            match error_message_3_part_1 lr with
            | Some input ->
              buggy_input := input;
              None
            | None ->
              match error_message_3_part_2 lr with
              | Some err -> Some (!buggy_input, err)
              | None ->
                match error_message_2 lr with
                | Some _path ->
                  (* This message is just noise preceeding a real syntax error message *)
                  Line_reader.next lr;
                  None
                | None ->
                  failwithf "driver: error: unexpected output: %S" line
        with End_of_file ->
          failwithf "driver: error: unexpected end of file after line: %S" line
      in
      match next with
      | Some _ as r -> r
      | None ->
        next_error buggy_input lr

  let read_errors lr =
    let buggy_input = ref "" in
    let rec loop acc =
      match next_error buggy_input lr with
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
  Filename.concat temp_dir (Printf.sprintf "ocamlgrammarfuzzer_%04d-%04d.%s" id i ext)

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
    let line_reader = Line_reader.make ?hook:debug_line pstderr in
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
