open Fix.Indexing
open Info
open Reachability

type ('g, 'r) t = private
  | Null of {
      cell: 'r cell index;
    }
  | Shift of {
      cell: 'r cell index;
      terminal: 'g terminal index;
    }
  | Node of {
      cell: 'r cell index;
      left: ('g, 'r) t;
      right: ('g, 'r) t;
      length: int;
    }
  | Expand of {
      cell: 'r cell index;
      expansion: ('g, 'r) t;
      reduction: 'g reduction;
    }

val cell : ('g, 'r) t -> 'r cell index
val length : ('g, 'r) t -> int
val iter_sub : (('g, 'r) t -> unit) -> ('g, 'r) t -> unit
val null : 'r cell index -> ('g, 'r) t
val shift : 'r cell index -> 'g terminal index -> ('g, 'r) t
val node : 'r cell index -> ('g, 'r) t -> ('g, 'r) t -> ('g, 'r) t
val expand : 'r cell index -> ('g, 'r) t -> 'g reduction -> ('g, 'r) t

val terminals : ('g, 'r) t -> 'g terminal index list
val iter_terminals : f:('g terminal index -> unit) -> ('g, 'r) t -> unit

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
val unroll_path : ('g, 'r) t -> ('g, 'r, ('g, 'r) t) path -> ('g, 'r) t
