module Error : sig
  type syntax = {
    line : int;
    start_col: int;
    end_col: int;
    message : string list;
  }

  type internal = string

  type t =
    | Syntax of syntax
    | Internal of internal

  val to_string : t -> string
end

val check : ?jobs:int -> ([`Impl | `Intf] * string) Seq.t -> Error.t list Seq.t
