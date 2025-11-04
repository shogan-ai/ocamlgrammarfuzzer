(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

open Fix.Indexing
open Utils
open Misc

type ('n, 'a) t
val make : 'n cardinal -> ('n, 'a) t
val add : ('n, 'a) t -> 'n indexset -> 'a -> unit
val pop : ('n, 'a) t -> ('n index * 'a list) option
val pop_seq : ('n, 'a) t -> ('n index * 'a list) Seq.t
