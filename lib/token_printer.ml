(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

open Fix.Indexing
open Utils
open Misc
open Info

let builtin = function
  | "AMPERAMPER"              -> "&&"
  | "AMPERSAND"               -> "&"
  | "AND"                     -> "and"
  | "ANDOP"                   -> "and*"
  | "AS"                      -> "as"
  | "ASSERT"                  -> "assert"
  | "AT"                      -> "@"
  | "ATAT"                    -> "@@"
  | "BACKQUOTE"               -> "`"
  | "BANG"                    -> "!"
  | "BAR"                     -> "|"
  | "BARBAR"                  -> "||"
  | "BARRBRACKET"             -> "|]"
  | "BEGIN"                   -> "begin"
  | "BORROW"                  -> "borrow_"
  | "CHAR"                    -> "'a'"
  | "CLASS"                   -> "class"
  | "COLON"                   -> ":"
  | "COLONCOLON"              -> "::"
  | "COLONEQUAL"              -> ":="
  | "COLONGREATER"            -> ":>"
  | "COLONRBRACKET"           -> ":]"
  | "COMMA"                   -> ","
  | "COMMENT"                 -> "(*comment %d*)"
  | "CONSTRAINT"              -> "constraint"
  | "DOCSTRING"               -> "(**documentation %d*)"
  | "DO"                      -> "do"
  | "DOLLAR"                  -> "$"
  | "DONE"                    -> "done"
  | "DOT"                     -> "."
  | "DOTDOT"                  -> ".."
  | "DOTHASH"                 -> ".#"
  | "DOTOP"                   -> ".+"
  | "DOWNTO"                  -> "downto"
  | "EFFECT"                  -> "effect"
  | "ELSE"                    -> "else"
  | "END"                     -> "end"
  | "EOF"                     -> ""
  | "EOL"                     -> "\n"
  | "EQUAL"                   -> "="
  | "EXCEPTION"               -> "exception"
  | "EXCLAVE"                 -> "exclave_"
  | "EXTERNAL"                -> "external"
  | "FALSE"                   -> "false"
  | "FLOAT"                   -> "%d.0"
  | "FOR"                     -> "for"
  | "FUNCTION"                -> "function"
  | "FUNCTOR"                 -> "functor"
  | "FUN"                     -> "fun"
  | "GLOBAL"                  -> "global_"
  | "GREATER"                 -> ">"
  | "GREATERRBRACE"           -> ">}"
  | "GREATERRBRACKET"         -> ">]"
  | "HASH"                    -> "#"
  | "HASH_CHAR"               -> "#'a'"
  | "HASH_FLOAT"              -> "#%d.0"
  | "HASH_INT"                -> "#%dl"
  | "HASHFALSE"               -> "#false"
  | "HASHTRUE"                -> "#true"
  | "HASHLBRACE"              -> "#{"
  | "HASHLPAREN"              -> "#("
  | "HASHOP"                  -> "##"
  | "HASH_SUFFIX"             -> "#"
  | "IF"                      -> "if"
  | "INCLUDE"                 -> "include"
  | "INFIXOP0"                -> "!="
  | "INFIXOP1"                -> "^"
  | "INFIXOP2"                -> "+!"
  | "INFIXOP3"                -> "land"
  | "INFIXOP4"                -> "**"
  | "INHERIT"                 -> "inherit"
  | "IN"                      -> "in"
  | "INITIALIZER"             -> "initializer"
  | "INT"                     -> "%d"
  | "KIND"                    -> "kind_"
  | "KIND_OF"                 -> "kind_of_"
  | "LABEL"                   -> "~label%d:"
  | "LAYOUT"                  -> "layout_"
  | "LAZY"                    -> "lazy"
  | "LBRACE"                  -> "{"
  | "LBRACELESS"              -> "{<"
  | "LBRACKET"                -> "["
  | "LBRACKETAT"              -> "[@"
  | "LBRACKETATAT"            -> "[@@"
  | "LBRACKETATATAT"          -> "[@@@"
  | "LBRACKETBAR"             -> "[|"
  | "LBRACKETCOLON"           -> "[:"
  | "LBRACKETGREATER"         -> "[>"
  | "LBRACKETLESS"            -> "[<"
  | "LBRACKETPERCENT"         -> "[%"
  | "LBRACKETPERCENTPERCENT"  -> "[%%"
  | "LESS"                    -> "<"
  | "LESSLBRACKET"            -> "<["
  | "LESSMINUS"               -> "<-"
  | "LET"                     -> "let"
  | "LETOP"                   -> "let*"
  | "LIDENT"                  -> "x%d"
  | "LOCAL"                   -> "local_"
  | "LPAREN"                  -> "("
  | "MATCH"                   -> "match"
  | "METAOCAML_BRACKET_CLOSE" -> ">."
  | "METAOCAML_BRACKET_OPEN"  -> ".<"
  | "METAOCAML_ESCAPE"        -> ".~"
  | "METHOD"                  -> "method"
  | "MINUS"                   -> "-"
  | "MINUSDOT"                -> "-."
  | "MINUSGREATER"            -> "->"
  | "MOD"                     -> "mod"
  | "MODULE"                  -> "module"
  | "MUTABLE"                 -> "mutable"
  | "NEW"                     -> "new"
  | "NONREC"                  -> "nonrec"
  | "OBJECT"                  -> "object"
  | "OF"                      -> "of"
  | "OPEN"                    -> "open"
  | "OPTLABEL"                -> "?label:"
  | "OR"                      -> "or"
  | "OVERWRITE"               -> "overwrite_"
  | "PARSER"                  -> "parser"
  | "PERCENT"                 -> "%"
  | "PLUS"                    -> "+"
  | "PLUSDOT"                 -> "+."
  | "PLUSEQ"                  -> "+="
  | "POLY"                    -> "poly_"
  | "PREFIXOP"                -> "!+"
  | "PRIVATE"                 -> "private"
  | "QUESTION"                -> "?"
  | "QUOTE"                   -> "'"
  | "QUOTED_STRING_EXPR"      -> "{%%ext|s%d|}"
  | "QUOTED_STRING_ITEM"      -> "{%%%%ext|s%d|}"
  | "RBRACE"                  -> "}"
  | "RBRACKET"                -> "]"
  | "RBRACKETGREATER"         -> "]>"
  | "REC"                     -> "rec"
  | "REPR"                    -> "repr_"
  | "RPAREN"                  -> ")"
  | "SEMI"                    -> ";"
  | "SEMISEMI"                -> ";;"
  | "SIG"                     -> "sig"
  | "STACK"                   -> "stack_"
  | "STAR"                    -> "*"
  | "STRING"                  -> "\"s%d\""
  | "STRUCT"                  -> "struct"
  | "THEN"                    -> "then"
  | "TILDE"                   -> "~"
  | "TO"                      -> "to"
  | "TRUE"                    -> "true"
  | "TRY"                     -> "try"
  | "TYPE"                    -> "type"
  | "UIDENT"                  -> "X%d"
  | "UNDERSCORE"              -> "_"
  | "VAL"                     -> "val"
  | "VIRTUAL"                 -> "virtual"
  | "WHEN"                    -> "when"
  | "WHILE"                   -> "while"
  | "WITH"                    -> "with"
  | "error" | "#" as x       -> x ^ "(*FIXME: Should not happen)"
  | _ -> raise Not_found

let no_space_before_token = function
  | "HASH_SUFFIX" -> true
  | _ -> false

let for_grammar (grammar : _ Info.grammar) custom =
  let (module G) = Info.raw grammar in
  let unknown = ref [] in
  let table =
    Vector.init (Terminal.cardinal grammar) @@
    fun t ->
    let name = Terminal.to_string grammar t in
    let kind = if no_space_before_token name then `Suffix else `Regular in
    let attributes = G.Terminal.attributes (G.Terminal.of_int (Index.to_int t)) in
    let text =
      match List.assoc_opt name custom with
      | Some txt -> txt
      | None ->
        match List.find_opt (fun attr -> G.Attribute.label attr = "name") attributes with
        | Some attr -> G.Attribute.payload attr
        | None ->
          match builtin name with
          | txt -> txt
          | exception Not_found ->
            push unknown name; name
    in
    let printer = match Terminal.semantic_value grammar t with
      | None -> fun _ -> text
      | Some _ ->
        match Scanf.format_from_string text "%d" with
        | fmt ->
          fun gensym -> Printf.sprintf fmt (gensym ())
        | exception (Scanf.Scan_failure txt)
          when String.starts_with ~prefix:"bad input: format type mismatch between " txt ->
          fun _ -> text
    in
    (printer, kind)
  in
  match !unknown with
  | [] -> table
  | xs ->
    prerr_endline "Unknown terminals (pass --terminal 'name=text'):";
    List.iter prerr_endline xs;
    exit 1
