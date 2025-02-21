[@@@warning "-33"]
open Fix.Indexing
open Utils
open Misc

module Grammar = MenhirSdk.Cmly_read.FromString(Ocaml_grammar)
module Info = Grammarfuzzer.Info.Make(Grammar)
module Reachability = Grammarfuzzer.Reachability.Make(Info)()
module Lrc_raw = Grammarfuzzer.Lrc.Make(Info)(Reachability)()
(*module Lrc = Grammarfuzzer.Lrc.From_entrypoints(Info)(Lrc_raw)(struct
    let entrypoints =
      Lrc_raw.lrcs_of_lr1 (Hashtbl.find Info.Lr1.entrypoints "implementation")
    end)
module Completion = Grammarfuzzer.Completion.Make(Info)(Lrc)

open Info

let () =
  let count = ref 0 in
  IndexSet.iter (fun lrc ->
      IndexSet.iter (fun tgt ->
          let lr1 = Lrc.lr1_of_lrc tgt in
          if Symbol.is_nonterminal (Option.get (Lr1.incoming lr1)) then
            incr count
        ) (Lrc.successors lrc)
    ) Lrc.reachable;
  Printf.eprintf "Count: %d\n" !count
*)
