(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

open Ocamlformat

val error_to_string : error -> string

val check :
  ?command:string ->
  ?extra_args:string list ->
  ?jobs:int ->
  ?batch_size:int ->
  ?debug_line:(string -> unit) ->
  (source_kind * string) Seq.t ->
  error list Seq.t
