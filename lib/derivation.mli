open Fix.Indexing
open Info
open Reachability

type ('g, 'r, 'a) t = private {
  cell: 'a;
  desc: ('g, 'r, 'a) desc;
}
and ('g, 'r, 'a) desc =
  | Null
  | Shift of 'g terminal index
  | Node of {
      left: ('g, 'r, 'a) t;
      right: ('g, 'r, 'a) t;
      length: int;
    }
  | Expand of {
      expansion: ('g, 'r, 'a) t;
      reduction: 'g reduction;
    }

val cell : ('g, 'r, 'a) t -> 'a
val length : ('g, 'r, 'a) t -> int
val iter_sub : (('g, 'r, 'a) t -> unit) -> ('g, 'r, 'a) t -> unit
val null : 'a -> ('g, 'r, 'a) t
val shift : 'a -> 'g terminal index -> ('g, 'r, 'a) t
val node : 'a -> ('g, 'r, 'a) t -> ('g, 'r, 'a) t -> ('g, 'r, 'a) t
val expand : 'a -> ('g, 'r, 'a) t -> 'g reduction -> ('g, 'r, 'a) t

val items_of_expansion :
  'g grammar ->
  expansion:('g, 'r, 'a) t ->
  reduction:'g reduction ->
  ('g, 'r, 'a * 'g item index) t

val get_terminal : ('g, 'r, 'a) t -> int -> 'g terminal index
val get_cells_on_path_to_index : ('g, 'r, 'a) t -> int -> 'a list

val terminals : ('g, 'r, 'a) t -> 'g terminal index list
val iter_terminals : f:('g terminal index -> unit) -> ('g, 'r, 'a) t -> unit

type ('g, 'r, 'a) path =
  | Left_of of {
      right: 'a;
      cell: 'r cell index;
    }
  | Right_of of {
      left: 'a;
      cell: 'r cell index;
    }
  | In_expansion of {
      reduction: 'g reduction;
      cell: 'r cell index;
    }

val map_path : ('a -> 'b) -> ('g, 'r, 'a) path -> ('g, 'r, 'b) path

val unroll_path :
  ('g, 'r, 'r cell index) t ->
  ('g, 'r, ('g, 'r, 'r cell index) t) path ->
  ('g, 'r, 'r cell index) t
