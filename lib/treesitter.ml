(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

open Utils.Misc
open Ocamlformat

let input_line ?(debug=ignore) ic =
  let result = input_line ic in
  debug result;
  result

let default_batch_size = 80

let temp_dir = Filename.get_temp_dir_name ()

let temp_path id i ext =
  Filename.concat temp_dir (Printf.sprintf "ocamlgrammarfuzzer_%04d-%04d.%s" id i ext)

let environ = lazy (Unix.environment ())

let batch_ids = ref 0

let start_batch ?debug_line ~command ~args = function
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
        command
        (Array.of_list (command :: args @ files))
        (Lazy.force environ)
    in
    Some (files, inputs, process)

let unlink_no_err path =
  try Unix.unlink path
  with _ -> ()

module Output_parser = struct

  let re_error = Str.regexp "(\\(.+\\) +\\[\\([0-9]+\\), *\\([0-9]+\\)\\] *- *\\[\\([0-9]+\\), *\\([0-9]+\\)\\])";;
  let extract_error s =
    if Str.string_match re_error s 0 then
      Some (Str.matched_group 1 s,
            int_of_string (Str.matched_group 2 s),
            int_of_string (Str.matched_group 3 s),
            int_of_string (Str.matched_group 4 s),
            int_of_string (Str.matched_group 5 s))
    else None

  let re_summary = Str.regexp "Total parses: \\([0-9]+\\); successful parses: \\([0-9]+\\); failed parses: \\([0-9]+\\); success percentage: \\([0-9.]+\\)%; average speed: \\([0-9]+\\) bytes/ms"

  let match_summary s =
    Str.string_match re_summary s 0

  let parse ?debug ic =
    let input_line ic = input_line ?debug ic in
    let trailer () =
      match input_line ic with
      | exception End_of_file ->
         failwith "unexpected end of file"
      | summary ->
         if not (match_summary summary) then
           failwith ("cannot parse summary: " ^ summary);
         match input_line ic with
         | "" ->
            begin match input_line ic with
            | exception End_of_file -> ()
            | otherwise ->
               failwith ("unexpected contents after response: " ^ otherwise)
            end
         | otherwise ->
            failwith ("expecting empty line after response, got: " ^ otherwise)
         | exception End_of_file ->
            failwith "unexpected end of file"
    in
    let rec loop acc =
      match input_line ic with
      | exception End_of_file -> failwith "unexpected end of file"
      | "" -> List.rev acc
      | line ->
         match String.split_on_char '\t' line with
         | [file; time; speed; error] ->
           let file = String.trim file in
           ignore time;
           ignore speed;
           let error = match extract_error error with
             | None -> failwith ("cannot parse error message: " ^ error)
             | Some error -> error
           in
           loop ((file, error) :: acc)
         | _ -> failwith ("ill-formed line: " ^ line)
    in
    let errors = loop [] in
    trailer ();
    errors

  let seek_pos text pos lines col =
    let rpos = ref pos in
    let rlines = ref lines in
    try
      while !rlines > 0 do
        rpos := String.index_from text !rpos '\n' + 1;
        decr rlines
      done;
      !rpos
    with Not_found ->
      let len = String.length text in
      if not (!rlines = 1 && col = 0) then
        Printf.eprintf "cannot find line %d in %S\n" lines
          (String.sub text pos (len - pos));
      len

  let locate_error contents (message, start_line, start_col, end_line, end_col) =
    let end_col =
      if start_line = end_line then
        end_col
      else
        let start_ofs = seek_pos contents 0 start_line 0 in
        seek_pos contents start_ofs (end_line - start_line) end_col - start_ofs
    in
    {message; location = Some {line = start_line + 1; start_col; end_col}}
end

  (*let locate_error acc (file, (message, start_line, start_col, end_line, end_col)) =
    let end_col, acc =
      if start_line = end_line then
        (end_col, acc)
      else
        match seek_input file acc with
        | None -> failwith ("cannot find test-case file: " ^ file)
        | Some ((_, contents), acc) ->
           let start_ofs = forward_lines contents 0 start_line in
           let end_ofs = forward_lines contents start_ofs (end_line - start_line) in
           ((end_ofs - start_ofs) + end_col, acc)
    in
    let location = Some {line = start_line; start_col; end_col} in
    ({message; location}, acc)*)
let consume_batch ?debug_line ~consume ~pack = function
  | None -> Seq.empty
  | Some (files, inputs, (pstdout, _, _ as process)) ->
    let errors = ref (Output_parser.parse ?debug:debug_line pstdout) in
    let rec annotate_error file input =
      match !errors with
      | [] -> []
      | (file', err) :: rest ->
         if file = file' then (
           errors := rest;
           Output_parser.locate_error (snd input) err :: annotate_error file input
         ) else
           []
    in
    ignore (Unix.close_process_full process);
    let errors = List.map2 annotate_error files inputs in
    List.to_seq (List.map2 (fun file errors -> pack (consume file) errors) files errors)

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
    ?(command="tree-sitter")
    ?(extra_args=[])
    ?(jobs=0) ?(batch_size=default_batch_size)
    ?debug_line
    seq
  =
  seq
  |> (* Group by batches of appropriate size *)
  batch_by ~size:batch_size
  |> (* Launch a process for each batch *)
  Seq.map (start_batch ?debug_line ~command
             ~args:("parse" :: "-q" :: "--stat" :: extra_args))
  |> (* Force sequence enough items ahead to kick [jobs] processes ahead *)
  overlapping_force jobs
  |> (* Collect the results *)
  Seq.concat_map (consume_batch ?debug_line
                    ~consume:unlink_no_err
                    ~pack:(fun () errors -> errors))
