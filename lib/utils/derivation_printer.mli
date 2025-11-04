(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

type 'a node

val node : string -> string node list -> string node

val layout_lines : string node list -> string list

val output : out_channel -> string node list -> unit
