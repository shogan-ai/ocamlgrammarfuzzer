let batch_size = 80

let () = Sys.chdir (Filename.get_temp_dir_name ())

let temp_path i = Printf.sprintf "ocamlgrammarfuzzer_%06d.ml" i

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
    let ic =
      Unix.open_process_args_in
        "ocamlformat"
        (Array.of_list ("ocamlformat" :: "--check" ::
                        "--enable-outside-detected-project" ::
                        files))
    in
    let _status = Unix.close_process_in ic in
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
