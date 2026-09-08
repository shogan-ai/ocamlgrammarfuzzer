(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

type location = {
    line : int;
    start_col: int;
    end_col: int;
}

type error = {
  message: string;
  location: location option;
}

val error_to_string : error -> string

type source_kind =
  | Impl
  | Intf

val check :
  ?command:string ->
  ?extra_args:string list ->
  ?jobs:int ->
  ?batch_size:int ->
  ?debug_line:(string -> unit) ->
  (source_kind * string) Seq.t ->
  error list Seq.t

val format :
  ?command:string ->
  ?jobs:int ->
  ?batch_size:int ->
  ?debug_line:(string -> unit) ->
  (source_kind * string) Seq.t ->
  (string * error list) Seq.t
