open Fix.Indexing
open Info
open Reachability

type ('g, 'r, 'm) t = private {
  meta: 'm;
  desc: ('g, 'r, 'm) desc;
}
and ('g, 'r, 'm) desc =
  | Null
  | Shift of 'g terminal index
  | Node of {
      left: ('g, 'r, 'm) t;
      right: ('g, 'r, 'm) t;
      length: int;
    }
  | Expand of {
      expansion: ('g, 'r, 'm) t;
      reduction: 'g reduction;
    }

val meta : ('g, 'r, 'm) t -> 'm
val length : ('g, 'r, _) t -> int
val iter_sub : (('g, 'r, 'm) t -> unit) -> ('g, 'r, 'm) t -> unit
val null : 'm -> ('g, 'r, 'm) t
val shift : 'm -> 'g terminal index -> ('g, 'r, 'm) t
val node : 'm -> ('g, 'r, 'm) t -> ('g, 'r, 'm) t -> ('g, 'r, 'm) t
val expand : 'm -> ('g, 'r, 'm) t -> 'g reduction -> ('g, 'r, 'm) t

val items_of_expansion :
  'g grammar ->
  expansion:('g, 'r, 'm) t ->
  reduction:'g reduction ->
  ('g, 'r, 'g item index list * 'm * 'g item index list) t

val get_terminal : ('g, 'r, 'm) t -> int -> 'g terminal index
val get_meta : ('g, 'r, 'm) t -> int -> 'm

val terminals : ('g, 'r, 'm) t -> 'g terminal index list
val iter_terminals : f:('g terminal index -> unit) -> ('g, 'r, 'm) t -> unit

type ('g, 'm, 'a) path =
  | Left_of of {
      right: 'a;
      meta: 'm;
    }
  | Right_of of {
      left: 'a;
      meta: 'm;
    }
  | In_expansion of {
      reduction: 'g Reachability.reduction;
      meta: 'm;
    }

val map_path : ('a -> 'b) -> ('g, 'm, 'a) path -> ('g, 'm, 'b) path

val unroll_path : ('g, 'r, 'm) t -> ('g, 'm, ('g, 'r, 'm) t) path -> ('g, 'r, 'm) t
