module Error : sig
  type t =
    | Syntax of {
      line : int;
      char_range : int * int;
      message : string list;
    }
    | Internal of { message : string; }
  val to_string : t -> string
end

val check : ([`Impl | `Intf] * string) Seq.t -> Error.t list Seq.t
