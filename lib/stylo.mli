(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

open Ocamlformat

val check :
  ?command:string ->
  ?extra_args:string list ->
  ?jobs:int ->
  ?batch_size:int ->
  ?debug_line:(string -> unit) ->
  (source_kind * string) Seq.t ->
  error list Seq.t
