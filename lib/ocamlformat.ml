open Utils.Misc

module Error = struct
  type t =
    | Syntax of {
        line: int;
        char_range: int * int;
        message: string list;
      }
    | Internal of {message: string}

  let to_string = function
    | Syntax {line; char_range = (s, e); message} ->
      Printf.sprintf "syntax error: line %d.%d-%d: %s"
        line s e (String.concat "\\n" message)
    | Internal {message} ->
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
      (fun input line scol ecol ->
         let char_range = (scol, ecol) in
         let rec message acc =
           let line = input_line ic in
           if String.starts_with ~prefix:"Error: " line ||
              (String.starts_with ~prefix:"  " line && acc = [])
           then List.rev (line :: acc)
           else message (line :: acc)
         in
         let message = message [] in
         (input, Error.Syntax {line; char_range; message})
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
      Some (Error.Internal {message})
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
      | (input, Error.Internal {message = "comment changed."}) :: rest -> (
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

let check_command = ["ocamlformat"; "--check"; "--enable-outside-detected-project"]

let batch_size = 80

let temp_dir = Filename.get_temp_dir_name ()

let temp_path i ext =
  Printf.sprintf "%s/ocamlgrammarfuzzer_%06d.%s" temp_dir i ext

let temp_path_index name =
  let name = Filename.remove_extension (Filename.basename name) in
  Scanf.sscanf_opt name "ocamlgrammarfuzzer_%06d" (fun index -> index)

let environ = lazy (Unix.environment ())

let check_batch = function
  | [] -> Seq.empty
  | inputs ->
    let files = List.mapi (fun i (kind, source) ->
        let ext = match kind with `Intf -> "mli" | `Impl -> "ml" in
        let path = temp_path i ext in
        let oc = open_out_bin path in
        output_string oc source;
        output_char oc '\n';
        close_out oc;
        path
      ) inputs
    in
    let (_pstdout, _pstdin, pstderr) as process =
      Unix.open_process_args_full
        (List.hd check_command)
        (Array.of_list (check_command @ files))
        (Lazy.force environ)
    in
    let errors =
      let finally () = ignore (Unix.close_process_full process) in
      match Output_parser.read_errors pstderr with
      | r -> finally (); r
      | exception exn -> finally (); raise exn
    in
    List.iter Unix.unlink files;
    let errors = Output_parser.rev_cleanup_errors errors in
    let answer = Array.make (List.length files) [] in
    List.iter begin fun (input, error) ->
      match temp_path_index input with
      | Some index -> answer.(index) <- error :: answer.(index)
      | None -> failwithf "driver: error: unexpected filename %S, \
                           expecting ocamlgrammarfuzzer_%%06d.{ml,mli}" input
    end errors;
    Array.to_seq answer

let check seq =
  Seq.concat_map check_batch (batch_by ~size:batch_size seq)
