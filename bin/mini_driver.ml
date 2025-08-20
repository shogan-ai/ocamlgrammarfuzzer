open Utils.Misc
open Grammarfuzzer

let () =
  match Sys.argv with
  | [|_; "-"|] ->
    let infer_kind sentence =
      let impl = "implementation: " in
      let intf = "interface: " in
      match string_chop_prefix ~prefix:impl sentence with
      | Some sentence -> (`Impl, sentence)
      | None ->
        match string_chop_prefix ~prefix:intf sentence with
        | Some sentence -> (`Intf, sentence)
        | None -> (`Impl, sentence)
    in
    read_lines stdin
    |> Seq.map infer_kind
    |> Ocamlformat.check
    |> Seq.iteri begin fun i errors ->
      List.iter begin fun error ->
        Printf.printf "line %06d: %s\n"
          i (Ocamlformat.Error.to_string error)
      end errors
    end
  | _ ->
    Printf.eprintf
      "Usage:\n\
      \  %s -\n\
       Read a list of OCaml sentences, one per line, on the standard input,\n\
       submit to OCamlformat, and check for errors.\n\
       \n\
       A sentence is a piece of OCaml code (e.g \"let x = 1\").\n\
       Prefix it by \"interface: \" to parse it as an interface\n\
       (default is \"implementation: \").\n\
      "
      Sys.argv.(0)
