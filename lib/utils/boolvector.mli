(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

open Fix.Indexing

type 'n t

val make : 'n cardinal -> bool -> 'n t
val init : 'n cardinal -> ('n index -> bool) -> 'n t
val test : 'n t -> 'n index -> bool
val set : 'n t -> 'n index -> unit
val clear : 'n t -> 'n index -> unit
val from_vector : ('n, 'a) vector -> ('a -> bool) -> 'n t
