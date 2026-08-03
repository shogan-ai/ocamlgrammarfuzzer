(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

type mark = { mutable used : bool; }
val new_mark : unit -> mark
val is_unused : mark -> bool
val cmon_mark : mark -> Cmon.t

type set
val empty : set
val is_empty : set -> bool
val singleton : mark -> set
val join : set -> set -> set
val mark_used : set -> unit
val cmon_set : set -> Cmon.t
