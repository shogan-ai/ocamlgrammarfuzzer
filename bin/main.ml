[@@@warning "-33"]
open Fix.Indexing
open Utils
open Misc

module Grammar = MenhirSdk.Cmly_read.FromString(Ocaml_grammar)
module Info = Grammarfuzzer.Info.Make(Grammar)
module Reachability = Grammarfuzzer.Reachability.Make(Info)()
module Lrc_raw = Grammarfuzzer.Lrc.Make(Info)(Reachability)()
