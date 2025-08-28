open Utils.Misc
open Grammarfuzzer

let opt_ocamlformat = ref "ocamlformat"

let spec_list = [
  ("--ocamlformat", Arg.Set_string opt_ocamlformat, " Path to OCamlformat command to use");
]

let usage_msg =
  "Usage: mini_driver [options] <input|->\n\n\
   Read a list of OCaml sentences, one per line, on the standard input,\n\
   submit to OCamlformat, and check for errors.\n\
   \n\
   A sentence is a piece of OCaml code (e.g \"let x = 1\").\n\
   Prefix it by \"interface: \" to parse it as an interface\n\
   (default is \"implementation: \").\n"

let input = ref ""

let () =
  Arg.parse spec_list (fun arg ->
      if !input = "" then
        input := arg
      else
        raise (Arg.Bad (Printf.sprintf "Unexpected argument %S" arg))
    ) usage_msg

let () = if !input = "" then (
    Arg.usage spec_list usage_msg;
    exit 1
  )

let ic = if !input = "-" then stdin else open_in_bin !input

let infer_kind sentence =
  let impl = "implementation: " in
  let intf = "interface: " in
  match string_chop_prefix ~prefix:impl sentence with
  | Some sentence -> (`Impl, sentence)
  | None ->
    match string_chop_prefix ~prefix:intf sentence with
    | Some sentence -> (`Intf, sentence)
    | None -> (`Impl, sentence)

let () =
  read_lines ic
  |> Seq.map infer_kind
  |> Ocamlformat.check ~ocamlformat_command:!opt_ocamlformat ~jobs:8
  |> Seq.iteri begin fun i errors ->
    List.iter begin fun error ->
      Printf.printf "line %06d: %s\n"
        i (Ocamlformat.Error.to_string error)
    end errors
  end

let () = if !input <> "-" then close_in ic
