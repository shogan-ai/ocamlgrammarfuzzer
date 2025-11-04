(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

(** An order chain is a data structure designed to incrementally construct a
    totally ordered set represented as an interval [0,n[.
    The order is progressively refined by accumulating constraints:
    - starting with a single element,
    - getting an element strictly larger than an existing one,
    - getting an element strictly larger than an existing one and strictly
      smaller than all existing elements that are larger.

    This data structure is used by the dynamic priority optimizations of LRGrep
    automata.
*)

(** Abstract type of an order chain *)
type t

(** An element of an order chain *)
type element

(** [make ()] creates a new singleton order chain. *)
val make : unit -> t

(** [root t] retrieves the root (smallest) element of the order chain [t]. *)
val root : t -> element

(** [next e] returns an element larger than [e] in the order chain.
    No new element is created if there already are larger ones. *)
val next : element -> element

(** [extend e] generates an element that is larger than [e] but strictly smaller
    than all elements in the chain that are larger than [e]. *)
val extend : element -> element

(** [freeze t] finalizes the order chain [t] and returns the cardinal of the
    set.
    It should be called after all necessary constraints have been accumulated.
    It can be called only once, and the chain cannot be extended after. *)
val freeze : t -> int

(** [evaluate e] returns the integer value represented by element [e].
    This function should be called only after the chain has been frozen.
    @raise Invalid_argument if [evaluate] is called on an element before the
    chain is frozen.
*)
val evaluate : element -> int
