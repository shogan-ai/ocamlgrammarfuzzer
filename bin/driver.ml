open Utils.Misc
open Grammarfuzzer

let () =
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
