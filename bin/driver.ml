let batch_size = 80

let temp_dir = Filename.get_temp_dir_name ()

let temp_path i = Printf.sprintf "%s/ocamlgrammarfuzzer_%06d.ml" temp_dir i
let temp_path_index name =
  Scanf.sscanf_opt (Filename.basename name) "ocamlgrammarfuzzer_%06d.ml" Fun.id

let failwithf fmt = Printf.ksprintf failwith fmt

let environ = Unix.environment ()

type 'a ocamlformat_error =
  | Syntax_error of {
      input: 'a;
      line: int;
      char_range: int * int;
      message: string list;
    }
  | Internal_error of {
      input: 'a;
      message: string;
    }

let map_error f = function
  | Syntax_error se -> Syntax_error {se with input = f se.input}
  | Internal_error ie -> Internal_error {ie with input = f ie.input}

let string_of_error = function
  | Syntax_error {input; line; char_range = (s, e); message} ->
    Printf.sprintf "syntax error: %s:%d.%d-%d: %s"
      input line s e (String.concat "\\n" message)
  | Internal_error {input; message} ->
    Printf.sprintf "internal error: %s: %s" input message

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
       Syntax_error {input; line; char_range; message})

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

let error_message_3_part_1 buggy_input text ic =
  Scanf.sscanf_opt text
    {|ocamlformat: Cannot process %S.|}
    (fun input ->
       match input_line ic with
       | "  Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues." ->
         buggy_input := input
       | line -> failwithf "driver: %s: unexpected error header: %s" input line
    )

let error_message_3_part_2 buggy_input text =
  let bug = "  BUG: " in
  if String.starts_with ~prefix:bug text then
    let bugl = String.length bug in
    let txtl = String.length text in
    let message = String.sub text bugl (txtl - bugl) in
    Some (Internal_error {input = !buggy_input; message})
  else
    None

let rec next_answer buggy_input ic =
  match input_line ic with
  | exception End_of_file -> None
  | "" -> next_answer buggy_input ic
  | line ->
    let next =
      try
        match error_message_1 line ic with
        | Some _ as r -> r
        | None ->
          match error_message_3_part_1 buggy_input line ic with
          | Some () ->
            None
          | None ->
            match error_message_3_part_2 buggy_input line with
            | Some _ as r -> r
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
    | None -> next_answer buggy_input ic

let read_answers ic =
  let buggy_input = ref "" in
  let rec loop acc =
    match next_answer buggy_input ic with
    | None -> List.rev acc
    | Some x -> loop (x :: acc)
  in
  loop []

let cleanup_answers answers =
  let rec loop acc = function
    | Internal_error {input; message = "comment changed."} :: rest ->
      begin match rest with
        | Syntax_error se :: _ when
            se.input = input &&
            String.starts_with ~prefix:"Error: comment (*" (List.hd se.message) ->
          loop acc rest
        | _ ->
          failwithf "driver: error: expecting error details after \"comment changed.\", got %s"
            (match rest with
             | [] -> "end of file"
             | e :: _ -> string_of_error e)
      end
    | x :: xs -> loop (x :: acc) xs
    | [] -> List.rev acc
  in
  loop [] answers

let ocamlformat_command = [|"ocamlformat"; "--check"; "--enable-outside-detected-project"|]

let process_batch ~get_source = function
  | [] -> []
  | inputs ->
    let inputs = Array.of_list inputs in
    let files = Array.mapi (fun i input ->
        let path = temp_path i in
        let oc = open_out_bin path in
        output_string oc (get_source input);
        output_char oc '\n';
        close_out oc;
        path
      ) inputs
    in
    let (_pstdout, _pstdin, pstderr) as process =
      Unix.open_process_args_full
        ocamlformat_command.(0)
        (Array.concat [ocamlformat_command; files])
        environ
    in
    let answers =
      let finally () = ignore (Unix.close_process_full process) in
      match read_answers pstderr with
      | r -> finally (); r
      | exception exn -> finally (); raise exn
    in
    Array.iter Unix.unlink files;
    let answers = cleanup_answers answers in
    List.map (map_error (fun filename ->
        match temp_path_index filename with
        | Some index -> (index, inputs.(index))
        | None ->
          failwithf
            "driver: error: unexpected filename %S, expecting ocamlgrammarfuzzer_%%06d.ml"
            filename
      )) answers

let read_lines ic =
  let rec loop () =
    match input_line ic with
    | line -> Seq.Cons (line, loop)
    | exception End_of_file -> Seq.Nil
  in
  loop

let batch_by ~size seq =
  assert (size > 0);
  let rec take acc n seq =
    if n = 0 then
      Seq.Cons (List.rev acc, start seq)
    else
      match seq () with
      | Seq.Nil -> Seq.Cons (List.rev acc, Seq.empty)
      | Seq.Cons (x, xs) -> take (x :: acc) (n - 1) xs
  and start seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, xs) ->
      take [x] (size - 1) xs
  in
  start seq

let () =
  let sentences = read_lines stdin in
  let batches = batch_by ~size:batch_size sentences in
  let offset = ref 1 in
  Seq.iter (fun batch ->
      let name_of (index, _) =
        Printf.sprintf "sentence %d" (!offset + index)
      in
      List.iter
        (fun err -> prerr_endline (string_of_error (map_error name_of err)))
        (process_batch ~get_source:Fun.id batch);
      offset := !offset + List.length batch;
    ) batches
