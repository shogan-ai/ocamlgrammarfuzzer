type location = {
    line : int;
    start_col: int;
    end_col: int;
}

type error = {
  message: string;
  location: location option;
}

val error_to_string : error -> string

type source_kind =
  | Impl
  | Intf

val check :
  ?ocamlformat_command:string ->
  ?jobs:int ->
  ?batch_size:int ->
  (source_kind * string) Seq.t ->
  error list Seq.t
