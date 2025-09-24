open Utils.Misc
open Grammarfuzzer

let opt_ocamlformat = ref "ocamlformat"
let opt_quiet = ref false
let opt_summary = ref false
let opt_success = ref false
let opt_raw_output = ref false
let opt_jobs = ref 8
let opt_batch_size = ref 400

let input = ref ""

let set_input arg =
  if !input = "" then
    input := arg
  else
    raise (Arg.Bad (Printf.sprintf
                      "Unexpected argument %S (input already set to %S)" arg !input))

let spec_list = [
  ("--ocamlformat", Arg.Set_string opt_ocamlformat, "<path> OCamlformat command to use");
  ("--silence-errors", Arg.Set opt_quiet, " Do not print errors");
  ("--print-success", Arg.Set opt_success, " Print line numbers that were successfully parsed");
  ("--jobs", Arg.Set_int opt_jobs, "<int> Number of ocamlformat processes to run in parallel (default: 8)");
  ("--batch-size", Arg.Set_int opt_batch_size, "<int> Number of files to submit to each ocamlformat process (default: 400)");
  ("--summary", Arg.Set opt_summary, " Output a summary at the end");
  ("--raw-output", Arg.Set opt_raw_output, " Output raw messages from ocamlformat on stderr");
  ("-", Arg.Unit (fun () -> set_input "-"), " Read from standard input");
]

let usage_msg =
  "Usage: ocamlgrammarfuzzer-minidriver [options] <path|->\n\n\
   Read a list of OCaml sentences, one per line, on the standard input of from a file, \
   submit them to OCamlformat and check for errors.\n\
   \n\
   A sentence is a piece of OCaml code (e.g \"let x = 1\").\n\
   Prefix it by \"interface: \" to parse it as an interface \
   (default is \"implementation: \").\n\
   \n\
   Exit with code 0 if no sentence failed, 1 otherwise.\n"

let () =
  Arg.parse spec_list set_input usage_msg

let () = if !input = "" then (
    Arg.usage spec_list usage_msg;
    exit 2
  )

let infer_kind sentence =
  let impl = "implementation: " in
  let intf = "interface: " in
  match string_chop_prefix ~prefix:impl sentence with
  | Some sentence -> (Ocamlformat.Impl, sentence)
  | None ->
    match string_chop_prefix ~prefix:intf sentence with
    | Some sentence -> (Ocamlformat.Intf, sentence)
    | None -> (Ocamlformat.Impl, sentence)

let success = ref 0
let failing = ref 0
let error_count = ref 0

let ic = if !input = "-" then stdin else open_in_bin !input

let () =
  read_lines ic
  |> Seq.map infer_kind
  |> Ocamlformat.check
    ~ocamlformat_command:!opt_ocamlformat
    ~jobs:(Int.max 0 !opt_jobs)
    ~batch_size:(Int.max 1 !opt_batch_size)
    ~debug_line:(if !opt_raw_output then prerr_endline else ignore)
  |> Seq.iteri begin fun i errors ->
    if not !opt_quiet then
      List.iter begin fun error ->
        Printf.printf "line %06d: %s\n"
          i (Ocamlformat.error_to_string error)
      end errors;
    match List.length errors with
    | 0 ->
      incr success;
      if !opt_success then
        Printf.printf "line %06d: success.\n" i;
    | n ->
      incr failing;
      error_count := !error_count + n
  end

let () = if !input <> "-" then close_in ic

let () =
  if !opt_summary then
    Printf.printf "successful sentences: %d, failing sentences: %d (errors: %d)\n"
      !success !failing !error_count

let () =
  if !failing > 0 then
    exit 1
  else
    exit 0
