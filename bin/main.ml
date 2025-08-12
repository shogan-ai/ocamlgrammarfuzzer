[@@@warning "-33"]
open Fix.Indexing
open Utils
open Misc

let opt_count = ref 1
let opt_length = ref 100
let opt_comments = ref false
let opt_seed = ref (-1)
let opt_jane = ref false

let spec_list = [
  ("-n"         , Arg.Set_int opt_count, "<int> Number of lines to generate"  );
  ("--count"    , Arg.Set_int opt_count, "<int> Number of lines to generate"  );
  ("-c"         , Arg.Set opt_comments , " Generate fake comments in the lines");
  ("--comments" , Arg.Set opt_comments , " Generate fake comments in the lines");
  ("-l"         , Arg.Set_int opt_length, "<int> Number of token per sentence");
  ("--length"   , Arg.Set_int opt_length, "<int> Number of token per sentence");
  ("--seed"     , Arg.Set_int opt_seed, "<int> Random seed");
  ("--jane"     , Arg.Set opt_jane, " Use Jane Street dialect of OCaml");
  ("-v"         , Arg.Unit (fun () -> incr Stopwatch.verbosity), " Increase verbosity");
]

let usage_msg = "Usage: ocamlgrammarfuzzer [options]"

let () =
  Arg.parse spec_list (fun unexpected ->
      raise (Arg.Bad (Printf.sprintf "Unexpected argument %S" unexpected))
    ) usage_msg

let () = match !opt_seed with
  | -1 -> Random.self_init ()
  |  n -> Random.init n

module Grammar = MenhirSdk.Cmly_read.FromString(struct
    let content =
      if !opt_jane then
        Ocaml_jane_grammar.content
      else
        Ocaml_grammar.content
  end)

open Grammarfuzzer

include Info.Lift(Grammar)

let reachability = Reachability.make grammar
module Reach = (val reachability)

open Info

let () = Random.self_init ()

let sample_list l =
  List.nth l (Random.int (List.length l))

let iter_sub_nodes ~f i_pre i_post l r =
  let coercion =
    Reach.Coercion.infix (Reach.Tree.post_classes l)
      (Reach.Tree.pre_classes r)
  in
  let l_index = Reach.Cell.encode l in
  let r_index = Reach.Cell.encode r in
  Array.iteri begin fun i_post_l all_pre_r ->
    let cl = l_index ~pre:i_pre ~post:i_post_l in
    let l_cost = Reach.Analysis.cost cl in
    if l_cost < max_int then
      let f' = f cl in
      Array.iter begin fun i_pre_r ->
        let cr = r_index ~pre:i_pre_r ~post:i_post in
        f' cr
      end all_pre_r
  end coercion.Reach.Coercion.forward

let iter_eqns ~f i_pre i_post goto =
  let tr = Transition.of_goto grammar goto in
  let c_pre = (Reach.Classes.pre_transition tr).(i_pre) in
  let c_post = (Reach.Classes.post_transition tr).(i_post) in
  let eqns = Reach.Tree.goto_equations goto in
  List.iter begin fun (reduction, node') ->
    if IndexSet.disjoint c_post reduction.Reach.lookahead then
      (* The post lookahead class does not permit reducing this
         production *)
      ()
    else
      match Reach.Tree.pre_classes node' with
      | [|c_pre'|] when IndexSet.disjoint c_pre' c_pre ->
        (* The pre lookahead class does not allow to enter this
           branch. *)
        ()
      | pre' ->
        (* Visit all lookahead classes, pre and post, and find
           the mapping between the parent node and this
           sub-node *)
        let pred_pre _ c_pre' =
          IndexSet.quick_subset c_pre' c_pre
        and pred_post _ c_post' =
          IndexSet.quick_subset c_post c_post'
        in
        match
          Misc.array_findi pred_pre 0 pre',
          Misc.array_findi pred_post 0 (Reach.Tree.post_classes node')
        with
        | exception Not_found -> ()
        | i_pre', i_post' ->
          f (Reach.Cell.encode node' ~pre:i_pre' ~post:i_post')
  end eqns.non_nullable

let rec fuzz size0 cell ~f =
  let current_cost = Reach.Analysis.cost cell in
  let size = Int.max size0 current_cost in
  let node, i_pre, i_post = Reach.Cell.decode cell in
  match Reach.Tree.split node with
  | R (l, r) ->
    let candidates = ref [] in
    iter_sub_nodes i_pre i_post l r ~f:(fun cl ->
        let l_cost = Reach.Analysis.cost cl in
        fun cr ->
          let r_cost = Reach.Analysis.cost cr in
          if r_cost < max_int && l_cost + r_cost <= size then
            push candidates (cl, cr)
      );
    let (cl, cr) = sample_list !candidates in
    let sl = Reach.Analysis.cost cl in
    let sr = Reach.Analysis.cost cr in
    let size = size - sl - sr in
    let mid =
      if Reach.Analysis.finite cl then
        0
      else if Reach.Analysis.finite cr then
        size
      else
        (Random.int (size + 1) + Random.int (size + 1)) / 2
    in
    fuzz (sl + mid) cl ~f;
    fuzz (sr + (size - mid)) cr ~f
  | L tr ->
    match Transition.split grammar tr with
    | R shift ->
      (* It is a shift transition, just shift the symbol *)
      f (Transition.shift_symbol grammar shift)
    | L goto ->
      (* It is a goto transition *)
      let eqns = Reach.Tree.goto_equations goto in
      let c_pre = (Reach.Tree.pre_classes node).(i_pre) in
      let c_post = (Reach.Tree.post_classes node).(i_post) in
      let nullable =
        (* Check if a nullable reduction is possible *)
        not (IndexSet.is_empty eqns.nullable_lookaheads) &&
        IndexSet.quick_subset c_post eqns.nullable_lookaheads &&
        not (IndexSet.disjoint c_pre c_post)
      in
      if size > 0 || not nullable then
        let candidates = ref [] in
        iter_eqns i_pre i_post goto ~f:(fun cell ->
            if Reach.Analysis.cost cell <= size then
              push candidates cell
          );
        let candidates = !candidates in
        if not nullable then
          fuzz size (sample_list candidates) ~f
        else
          let length = List.length candidates in
          let index = Random.int (1 + length * (size + 1)) in
          if index > 0 then
            fuzz size (List.nth candidates ((index - 1) / (size + 1))) ~f

let () = Misc.stopwatch 0 "Start BFS"

let bfs = Vector.make Reach.Cell.n ([], [])

let () =
  let todo = ref [] in
  let visit parent cell =
    match bfs.:(cell) with
    | (_::_, _) | (_, _::_ ) -> ()
    | ([], []) ->
      bfs.:(cell) <- parent;
      push todo cell
  in
  let propagate cell =
    let prefix, suffix as path = bfs.:(cell) in
    let node, i_pre, i_post = Reach.Cell.decode cell in
    match Reach.Tree.split node with
    | R (l, r) ->
      iter_sub_nodes ~f:(fun cl cr ->
          visit (prefix, cr :: suffix) cl;
          visit (cl :: prefix, suffix) cr;
        ) i_pre i_post l r
    | L tr ->
      match Transition.split grammar tr with
      | R _ -> ()
      | L gt ->
        iter_eqns i_pre i_post gt ~f:(visit path)
  in
  IndexSet.iter (fun tr ->
      let node = Reach.Tree.leaf (Transition.of_goto grammar tr) in
      visit ([],[]) (Reach.Cell.encode node ~pre:0 ~post:0)
    ) (Transition.accepting grammar);
  let counter = ref 0 in
  fixpoint ~counter ~propagate todo;
  Misc.stopwatch 0 "Stop BFS (depth: %d)" !counter

let unknown = ref []

let terminals = Vector.init (Terminal.cardinal grammar) @@
  fun t -> match Terminal.to_string grammar t with
  | "AMPERAMPER"             -> "&&"
  | "AMPERSAND"              -> "&"
  | "AND"                    -> "and"
  | "AS"                     -> "as"
  | "ASSERT"                 -> "assert"
  | "BACKQUOTE"              -> "`"
  | "BANG"                   -> "!"
  | "BAR"                    -> "|"
  | "BARBAR"                 -> "||"
  | "BARRBRACKET"            -> "|]"
  | "BEGIN"                  -> "begin"
  | "CHAR"                   -> "'a'"
  | "CLASS"                  -> "class"
  | "COLON"                  -> ":"
  | "COLONCOLON"             -> "::"
  | "COLONEQUAL"             -> ":="
  | "COLONGREATER"           -> ":>"
  | "COMMA"                  -> ","
  | "CONSTRAINT"             -> "constraint"
  | "DO"                     -> "do"
  | "DONE"                   -> "done"
  | "DOT"                    -> "."
  | "DOTDOT"                 -> ".."
  | "DOWNTO"                 -> "downto"
  | "EFFECT"                 -> "effect"
  | "ELSE"                   -> "else"
  | "END"                    -> "end"
  | "EOF"                    -> ""
  | "EQUAL"                  -> "="
  | "EXCEPTION"              -> "exception"
  | "EXTERNAL"               -> "external"
  | "FALSE"                  -> "false"
  | "FLOAT"                  -> "42.0"
  | "FOR"                    -> "for"
  | "FUN"                    -> "fun"
  | "FUNCTION"               -> "function"
  | "FUNCTOR"                -> "functor"
  | "GREATER"                -> ">"
  | "GREATERRBRACE"          -> ">}"
  | "GREATERRBRACKET"        -> ">]"
  | "IF"                     -> "if"
  | "IN"                     -> "in"
  | "INCLUDE"                -> "include"
  | "INFIXOP0"               -> "!="
  | "INFIXOP1"               -> "@"
  | "INFIXOP2"               -> "+!"
  | "INFIXOP3"               -> "land"
  | "INFIXOP4"               -> "**"
  | "DOTOP"                  -> ".+"
  | "LETOP"                  -> "let*"
  | "ANDOP"                  -> "and*"
  | "INHERIT"                -> "inherit"
  | "INITIALIZER"            -> "initializer"
  | "INT"                    -> "42"
  | "LABEL"                  -> "~label:"
  | "LAZY"                   -> "lazy"
  | "LBRACE"                 -> "{"
  | "LBRACELESS"             -> "{<"
  | "LBRACKET"               -> "["
  | "LBRACKETBAR"            -> "[|"
  | "LBRACKETLESS"           -> "[<"
  | "LBRACKETGREATER"        -> "[>"
  | "LBRACKETPERCENT"        -> "[%"
  | "LBRACKETPERCENTPERCENT" -> "[%%"
  | "LESS"                   -> "<"
  | "LESSMINUS"              -> "<-"
  | "LET"                    -> "let"
  | "LIDENT"                 -> "lident"
  | "LPAREN"                 -> "("
  | "LBRACKETAT"             -> "[@"
  | "LBRACKETATAT"           -> "[@@"
  | "LBRACKETATATAT"         -> "[@@@"
  | "MATCH"                  -> "match"
  | "METHOD"                 -> "method"
  | "MINUS"                  -> "-"
  | "MINUSDOT"               -> "-."
  | "MINUSGREATER"           -> "->"
  | "MODULE"                 -> "module"
  | "MUTABLE"                -> "mutable"
  | "NEW"                    -> "new"
  | "NONREC"                 -> "nonrec"
  | "OBJECT"                 -> "object"
  | "OF"                     -> "of"
  | "OPEN"                   -> "open"
  | "OPTLABEL"               -> "?label:"
  | "OR"                     -> "or"
  | "PARSER"                 -> "parser"
  | "PERCENT"                -> "%"
  | "PLUS"                   -> "+"
  | "PLUSDOT"                -> "+."
  | "PLUSEQ"                 -> "+="
  | "PREFIXOP"               -> "!+"
  | "PRIVATE"                -> "private"
  | "QUESTION"               -> "?"
  | "QUOTE"                  -> "'"
  | "RBRACE"                 -> "}"
  | "RBRACKET"               -> "]"
  | "REC"                    -> "rec"
  | "RPAREN"                 -> ")"
  | "SEMI"                   -> ";"
  | "SEMISEMI"               -> ";;"
  | "HASH"                   -> "#"
  | "HASHOP"                 -> "##"
  | "SIG"                    -> "sig"
  | "STAR"                   -> "*"
  | "STRING"                 -> "\"hello\""
  | "QUOTED_STRING_EXPR"     -> "{%hello|world|}"
  | "QUOTED_STRING_ITEM"     -> "{%%hello|world|}"
  | "STRUCT"                 -> "struct"
  | "THEN"                   -> "then"
  | "TILDE"                  -> "~"
  | "TO"                     -> "to"
  | "TRUE"                   -> "true"
  | "TRY"                    -> "try"
  | "TYPE"                   -> "type"
  | "UIDENT"                 -> "UIdent"
  | "UNDERSCORE"             -> "_"
  | "VAL"                    -> "val"
  | "VIRTUAL"                -> "virtual"
  | "WHEN"                   -> "when"
  | "WHILE"                  -> "while"
  | "WITH"                   -> "with"
  | "COMMENT"                -> "(*comment*)"
  | "DOCSTRING"              -> "(**documentation*)"
  | "EOL"                    -> "\n"
  | "METAOCAML_ESCAPE"       -> ".~"
  | "METAOCAML_BRACKET_OPEN" -> ".<"
  | "METAOCAML_BRACKET_CLOSE" -> ">."
  | "error" | "#" as x       -> x ^ "(*FIXME: Should not happen)"
  | "MOD"           -> "mod"
  | "EXCLAVE"       -> "exclave_"
  | "GLOBAL"        -> "global_"
  | "KIND_ABBREV"   -> "kind_abbrev_"
  | "KIND_OF"       -> "kind_of_"
  | "LOCAL"         -> "local_"
  | "ONCE"          -> "once_"
  | "OVERWRITE"     -> "overwrite_"
  | "STACK"         -> "stack_"
  | "UNIQUE"        -> "unique_"
  | "LBRACKETCOLON" -> "[:"
  | "HASH_SUFFIX"   -> "#"
  | "HASH_INT"      -> "#1"
  | "HASH_FLOAT"    -> "#1.0"
  | "HASHLPAREN"    -> "#("
  | "AT"            -> "@"
  | "ATAT"          -> "@@"
  | "COLONRBRACKET" -> ":]"
  | "DOTHASH"       -> ".#"
  | "HASHLBRACE"    -> "#{"
  | x -> push unknown x; x

let () = match !unknown with
  | [] -> ()
  | xs ->
    prerr_endline "Unknown terminals:";
    List.iter prerr_endline xs;
    exit 1

let generate_sentence ?(length=100) ?from f =
  let tr = match from with
    | None -> IndexSet.choose (Transition.accepting grammar)
    | Some tr -> tr
  in
  let tr = Transition.of_goto grammar tr in
  assert (Array.length (Reach.Classes.pre_transition tr) = 1);
  assert (Array.length (Reach.Classes.post_transition tr) = 1);
  let node = Reach.Tree.leaf tr in
  let cell = Reach.Cell.encode node ~pre:0 ~post:0 in
  fuzz length cell ~f

let output_with_comments oc =
  let count = ref 0 in
  fun t ->
    if !count > 0 then output_char oc ' ';
    Printf.fprintf oc "(* C%d *) %s" !count terminals.:(t);
    incr count

let directly_output oc =
  let need_sep = ref false in
  fun t ->
    if !need_sep then output_char oc ' ';
    need_sep := true;
    output_string oc terminals.:(t)

let () =
  Printf.eprintf "%d cells\n" (cardinal Reach.Cell.n)

let () =
  for _ = 0 to !opt_count - 1 do
    generate_sentence ~length:!opt_length
      (if !opt_comments
       then output_with_comments stdout
       else directly_output stdout);
    output_char stdout '\n'
  done
