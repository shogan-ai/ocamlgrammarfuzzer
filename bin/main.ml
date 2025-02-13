
module Grammar = MenhirSdk.Cmly_read.FromString(Ocaml_grammar)
module Info = Grammarfuzzer.Info.Make(Grammar)
module Reachability = Grammarfuzzer.Reachability.Make(Info)()
module Lrc_raw = Grammarfuzzer.Lrc.Make(Info)(Reachability)
module Lrc = Grammarfuzzer.Lrc.From_entrypoints(Info)(Lrc_raw)(struct
    let entrypoints =
      Lrc_raw.lrcs_of_lr1 (Hashtbl.find Info.Lr1.entrypoints "implementation")
    end)
module Completion = Grammarfuzzer.Completion.Make(Info)(Lrc)
