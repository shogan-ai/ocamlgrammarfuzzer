let batch_size = 80

let () = Sys.chdir (Filename.get_temp_dir_name ())

let temp_path i = Printf.sprintf "ocamlgrammarfuzzer_%06d.ml" i

let environ = Unix.environment ()

(* Error message shape 1:

  File "%s", line %d, characters %d-%d:
  Error: %s
*)

let error_message_1 text ic =
  Scanf.sscanf_opt text
    {|File %S, line %d, characters %d-%d:|}
    (fun path line scol ecol ->
       (path, line, scol, ecol, input_line ic))

(* Error message shape 2:

   ocamlformat: ignoring "%s" (syntax error)
*)

let error_message_2 text =
  Scanf.sscanf_opt text
    {|ocamlformat: ignoring %S (syntax error)|}
    (fun path -> path)

(* Error message shape 3:

   ocamlformat: Cannot process "ocamlgrammarfuzzer_002160.ml".
     Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.
     BUG: ast changed.
*)

let error_message_3 text ic =
  Scanf.sscanf_opt text
    {|ocamlformat: Cannot process %S.|}
    (fun path ->
       match input_line ic with
       | "  Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues." ->
         begin match input_line ic with
           | "  BUG: ast changed."
           | "  BUG: generating invalid ocaml syntax."
           | "  BUG: comment changed."
           | "  BUG: formatting did not stabilize after 10 iterations."
             as bug ->
             (path, bug)
           | line -> failwith ("Unexpected error line 2: " ^ line)
         end
       | line -> failwith ("Unexpected error line 1: " ^ line))

let rec parse_answer ic =
  match input_line ic with
  | "" -> parse_answer ic
  | line ->
    match error_message_1 line ic with
    | Some (path, lnum, scol, ecol, message) ->
      Printf.eprintf "  SYNTAX ERROR %s:%d.%d-%d: %s\n" path lnum scol ecol message
    | None ->
      match error_message_2 line with
      | Some _path ->
        (* This is just a comment before a SYNTAX ERROR *)
        ()
      | None ->
        match error_message_3 line ic with
        | Some (path, bug) ->
          Printf.eprintf "INTERNAL ERROR %s: %s\n" path bug
        | None -> ()

let process_output ic =
  try
    while true do
      parse_answer ic
    done
  with End_of_file -> ()



let process_batch offset = function
  | [] -> ()
  | lines ->
    let files = List.mapi (fun i line ->
        let path = temp_path (offset + i) in
        let oc = open_out_bin path in
        output_string oc line;
        output_char oc '\n';
        close_out oc;
        path
      ) lines
    in
    let (_pstdout, _pstdin, pstderr) as process =
      Unix.open_process_args_full
        "ocamlformat"
        (Array.of_list ("ocamlformat" :: "--check" ::
                        "--enable-outside-detected-project" ::
                        files))
        environ
    in
    process_output pstderr;
    let _status = Unix.close_process_full process in
    List.iter (fun file -> Unix.unlink file) files

let rec loop offset count acc ic =
  match input_line ic with
  | line ->
    let acc = line :: acc in
    let count = count + 1 in
    let acc, count, offset =
      if count = batch_size then (
        process_batch offset (List.rev acc);
        ([], 0, offset + count)
      ) else
        (acc, count, offset)
    in
    loop offset count acc ic
  | exception End_of_file ->
    process_batch offset (List.rev acc)

let () = loop 0 0 [] stdin
