(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

(** Module for printing text with line tracking and location output.
    This is useful for compilers and preprocessors that need to generate code
    that refers to locations in external files.
    The module allows tracking of code positions and can output directives indicating
    where the code was originally located in the source files. *)

(** Opaque type representing the state of the code printer. *)
type t

(** [create ~filename ?line output_function] creates a new code printer.
    [filename] is the name of the output file.
    [line] is the line number at which output starts to be appended (defaults to 1).
    [output_function] is called to append a string to the output file.
*)
val create : filename:string -> ?line:int -> (string -> unit) -> t

(** [print t ?loc text] appends [text] to [t].
    If [loc] is provided, a directive
      # <loc.start_line> <loc.loc_file>
    is emitted to indicate that [text] was extracted from [loc].
    If [loc] is not provided, a directive
      # <output filename> <output current line number>
    is emitted if necessary to indicate that [text] is code specific to the
    printed file.
*)
val print : ?loc:Lexing.position -> t -> string -> unit

(** [fmt] is a variant of [print] that supports [Printf]-like format strings. *)
val fmt : ?loc:Lexing.position -> t -> ('a, unit, string, unit) format4 -> 'a
