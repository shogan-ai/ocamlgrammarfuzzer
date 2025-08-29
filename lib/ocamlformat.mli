module Error : sig
  type syntax = {
    line : int;
    start_col: int;
    end_col: int;
    message : string;
  }

  type internal = string

  type t =
    | Syntax of syntax
    | Internal of internal
    | Comment_dropped of int

  val to_string : t -> string
end

val check :
  ?ocamlformat_command:string ->
  ?jobs:int ->
  ([`Impl | `Intf] * string) Seq.t ->
  Error.t list Seq.t
