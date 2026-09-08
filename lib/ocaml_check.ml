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

let error ?location message = {message; location}

let error_to_string {message; location} =
  match location with
  | None ->
    Printf.sprintf "error: %s" message
  | Some {line; start_col; end_col} ->
    Printf.sprintf "error: line %d.%d-%d: %s"
      line start_col end_col message

let re_file = Str.regexp "File \\\"\\([^\"]+\\)\\\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):$"

let prefix_error = "Error: "

let parse_line line =
  if Str.string_match re_file line 0 then
    let fname = Str.matched_group 1 line in
    let lnum = int_of_string (Str.matched_group 2 line) in
    let scol = int_of_string (Str.matched_group 3 line) in
    let ecol = int_of_string (Str.matched_group 4 line) in
    `File (fname, lnum, scol, ecol)
  else if String.starts_with ~prefix:prefix_error line then
    let lp = String.length prefix_error in
    `Error (String.sub line lp (String.length line - lp))
  else
    `None

let default_batch_size = 300

let temp_dir = Filename.get_temp_dir_name ()

let temp_path id i ext =
  Filename.concat temp_dir (Printf.sprintf "ocamlgrammarfuzzer_%04d-%04d.%s" id i ext)

let temp_path_index name =
  let name = Filename.remove_extension (Filename.basename name) in
  try Some (Scanf.sscanf name "ocamlgrammarfuzzer_%d-%d" (fun _id index -> index))
  with Scanf.Scan_failure _ | Failure _ | End_of_file ->
    None

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
    Some (files, process)

let unlink_no_err path =
  try Unix.unlink path
  with _ -> ()

let consume_batch ?(debug_line=ignore) ~consume ~pack = function
  | None -> Seq.empty
  | Some (files, (_, _, pstderr as process)) ->
    let errors = ref [] in
    let flush = function
      | None -> ()
      | Some (filename, location) ->
        push errors (filename, error ~location "")
    in
    let parse_lines acc line =
      debug_line line;
      match parse_line line with
      | `Error message ->
        begin match acc with
          | Some (filename, location) ->
            push errors (filename, error ~location message)
          | None -> ()
        end;
        None
      | `File (filename, line, start_col, end_col) ->
        flush acc;
        Some (filename, {line; start_col; end_col})
      | `None -> acc
    in
    flush (In_channel.fold_lines parse_lines None pstderr);
    ignore (Unix.close_process_full process);
    let files = Array.of_list (List.map consume files) in
    let answer = Array.make (Array.length files) [] in
    List.iter begin fun (input, error) ->
      match temp_path_index input with
      | Some index -> answer.(index) <- error :: answer.(index)
      | None -> failwithf "driver: error: unexpected filename %S, \
                           expecting ocamlgrammarfuzzer_%%06d.{ml,mli}" input
    end !errors;
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
    ?(command="ocamlc")
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
             ~args:("-stop-after" :: "parsing"
                    :: extra_args))
  |> (* Force sequence enough items ahead to kick [jobs] processes ahead *)
  overlapping_force jobs
  |> (* Collect the results *)
  Seq.concat_map (consume_batch ?debug_line
                    ~consume:unlink_no_err
                    ~pack:(fun () errors -> errors))
