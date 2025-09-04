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

type source_kind =
  | Impl
  | Intf

val check :
  ?ocamlformat_command:string ->
  ?jobs:int ->
  ?batch_size:int ->
  (source_kind * string) Seq.t ->
  Error.t list Seq.t
