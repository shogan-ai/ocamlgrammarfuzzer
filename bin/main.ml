[@@@warning "-33"]
open Fix.Indexing
open Utils
open Misc
open Grammarfuzzer

let opt_count = ref 1
let opt_length = ref 100
let opt_comments = ref false
let opt_seed = ref (-1)
let opt_oxcaml = ref false
let opt_entrypoints = ref []
let opt_print_entrypoint = ref false
let opt_weights = ref []
let opt_avoid = ref []
let opt_focus = ref []
let opt_exhaust = ref false

let spec_list = [
  (* ("-n"         , Arg.Set_int opt_count, "<int> Number of lines to generate"  ); *)
  ("--count"    , Arg.Set_int opt_count, "<int> Number of lines to generate"  );
  (* ("-c"         , Arg.Set opt_comments , " Generate fake comments in the lines"); *)
  ("--comments" , Arg.Set opt_comments , " Generate fake comments in the lines");
  (* ("-l"         , Arg.Set_int opt_length, "<int> Number of token per sentence"); *)
  ("--length"   , Arg.Set_int opt_length, "<int> Number of token per sentence");
  ("--seed"     , Arg.Set_int opt_seed, "<int> Random seed");
  ("--oxcaml"   , Arg.Set opt_oxcaml, " Work with Oxcaml dialect");
  ("-v"         , Arg.Unit (fun () -> incr Misc.verbosity_level), " Increase verbosity");
  ("--entrypoint", Arg.String (push opt_entrypoints), " Generate sentences from this entrypoint");
  ("--print-entrypoint", Arg.Set opt_print_entrypoint, " Prefix every sentence by the entrypoint followed by ':'");
  ("--weight", Arg.String (push opt_weights), " Adjust the weights of grammatical constructions");
  ("--avoid", Arg.String (push opt_avoid), " Forbid grammatical constructions");
  ("--focus", Arg.String (push opt_focus), " Generate sentences stressing a grammatical construction");
  ("--exhaust", Arg.Set opt_exhaust, " Exhaust mode generates a deterministic set of sentences that cover all reachable constructions");
]

let usage_msg = "Usage: ocamlgrammarfuzzer [options]"

let () =
  Arg.parse spec_list (fun unexpected ->
      raise (Arg.Bad (Printf.sprintf "Unexpected argument %S" unexpected))
    ) usage_msg

let () = match !opt_seed with
  | -1 -> Random.self_init ()
  |  n -> Random.init n

(* Load and preprocess the grammar *)

module Grammar = MenhirSdk.Cmly_read.FromString(struct
    let content =
      if !opt_oxcaml
      then Ocaml_jane_grammar.content
      else Ocaml_grammar.content
  end)

include Info.Lift(Grammar)
open Info

let point_error_position (pat : string) pos =
  let lines = String.split_on_char '\n' pat in
  let rec loop i = function
    | x :: xs when i < pos.Lexing.pos_lnum ->
      x :: loop (i + 1) xs
    | otherwise ->
      (String.make (pos.pos_cnum - pos.pos_bol) ' ' ^ "^") ::
      otherwise
  in
  loop 0 lines

let pattern_pos = {
  Lexing.pos_fname = "<pattern>";
  pos_lnum = 1;
  pos_cnum = 0;
  pos_bol = 0;
}

let prepare_lexer pattern =
  let st = Front.Lexer.fresh_state () in
  let lb = Front.Lexer.prepare_lexbuf st (Lexing.from_string pattern) in
  Lexing.set_filename lb "<pattern>";
  (st, lb)

exception Error of Lexing.position * string

let raise_error pos msg =
  raise (Error (pos, msg))

let raise_errorf pos fmt =
  Printf.ksprintf (raise_error pos) fmt

let parse_string_with pattern f =
  let st, lb = prepare_lexer pattern in
  match f (Front.Lexer.main st) lb with
  | result -> result
  | exception exn ->
    let msg, pos = match exn with
      | Front.Lexer.Error {msg; pos} -> (msg, pos)
      | Front.Parser.Error -> "syntax error", lb.Lexing.lex_start_p
      | _ -> raise exn
    in
    let lines = point_error_position pattern pos in
    raise_errorf pos "%s.\n  %s" msg (String.concat "\n  " lines)

let parse_pattern pattern =
  parse_string_with pattern Front.Parser.parse_filter

let parse_weight pattern =
  parse_string_with pattern (fun lexer lexbuf ->
      match lexer lexbuf with
      | Front.Parser.NUMBER weight ->
        let weight = float_of_string weight in
        let pattern = Front.Parser.parse_filter lexer lexbuf in
        (weight, pattern)
      | _ -> raise_error lexbuf.lex_start_p
               "expecting a weight value (a number)"
    )

(* Parse weight specifications *)

let weights = Vector.make (Production.cardinal grammar) 1.0

let symbol_index = Transl.Indices.make grammar

let transl_filter = function
  | {Syntax. lhs = None; rhs = [Syntax.Find (Some symbol), pos]} ->
    (* Special-case: look for all occurrences of a single symbol *)
    Either.Left (Transl.Indices.get_symbol symbol_index pos symbol)
  | filter ->
    Either.Right (Transl.transl_filter_to_prods
                    grammar symbol_index pattern_pos filter)

let process_weight (weight, filter) =
  let update_prod prod = weights.@(prod) <- ( *. ) weight in
  match transl_filter filter with
  | Either.Left sym ->
    Index.iter (Production.cardinal grammar) (fun prod ->
        let rhs = Production.rhs grammar prod in
        if Array.exists (Index.equal sym) rhs then
          update_prod prod
      )
  | Either.Right prods ->
    List.iter (fun (prod, _) -> update_prod prod) prods

let () =
  List.iter (fun spec -> process_weight (parse_weight spec))
    (List.rev !opt_weights);
  List.iter (fun spec -> process_weight (0.0, parse_pattern spec))
    (List.rev !opt_avoid)

(* Compute reachability, considering only productions with strictly positive
   weights. *)

let reachability = Reachability.make grammar ~avoid:(fun prod -> weights.:(prod) <= 0.0)

module Reach = (val reachability)

let () = stopwatch 1 "reachability (%d cells)" (cardinal Reach.Cell.n)

(* Naive fuzzing *)

let sample_list = function
  | [] -> failwith "empty list"
  | l ->
  let total = List.fold_left (fun sum (_, w) -> sum +. w) 0.0 l in
  let sample = ref (Random.float total) in
  fst (List.find (fun (_, w) ->
      sample := !sample -. w;
      !sample <= 0.0
    ) l)

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
          f reduction (Reach.Cell.encode node' ~pre:i_pre' ~post:i_post')
  end eqns.non_nullable

type derivation =
  | Null of {
      cell: Reach.Cell.n index;
    }
  | Shift of {
      cell: Reach.Cell.n index;
      terminal: g terminal index;
    }
  | Node of {
      cell: Reach.Cell.n index;
      left: derivation;
      right: derivation;
      length: int;
    }
  | Expand of {
      cell: Reach.Cell.n index;
      expansion: derivation;
      reduction: Reach.reduction;
    }

let derivation_cell ( Null {cell} | Shift {cell; _}
                    | Node {cell; _} | Expand {cell; _}) =
  cell

let rec derivation_length = function
  | Null _ -> 0
  | Shift _ -> 1
  | Node {length; _} -> length
  | Expand {expansion; _} -> derivation_length expansion

let derivation_iter_sub f = function
  | Null _ | Shift _ -> ()
  | Node {left; right; _} -> f left; f right
  | Expand {expansion; _} -> f expansion

let derivation_node cell left right =
  let length = derivation_length left + derivation_length right in
  Node {cell; left; right; length}

exception Derivation_found of derivation

let min_sentence =
  let solve = lazy (
    let sentinel = Null {cell = Index.of_int Reach.Cell.n 0} in
    let table = Vector.make Reach.Cell.n sentinel in
    let rec solve cell =
      match table.:(cell) with
      | result when result != sentinel -> result
      | _ ->
        let cost = Reach.Analysis.cost cell in
        if cost = max_int then
          failwith "min_sentence: unreachable cell";
        let result =
          if cost = 0 then
            Null {cell}
          else
            let node, i_pre, i_post = Reach.Cell.decode cell in
            try
              match Reach.Tree.split node with
              | R (l, r) ->
                iter_sub_nodes i_pre i_post l r ~f:(fun cl ->
                    let l_cost = Reach.Analysis.cost cl in
                    fun cr ->
                      let r_cost = Reach.Analysis.cost cr in
                      if l_cost < max_int && r_cost < max_int &&
                         l_cost + r_cost = cost then
                        let der =
                          derivation_node cell (solve cl) (solve cr)
                        in
                        raise (Derivation_found der)
                  );
                assert false
              | L tr ->
                match Transition.split grammar tr with
                | R shift ->
                  assert (cost = 1);
                  (* We reached a shift transition *)
                  let terminal = Transition.shift_symbol grammar shift in
                  Shift {cell; terminal}
                | L goto ->
                  iter_eqns i_pre i_post goto ~f:(fun reduction cell ->
                      if Reach.Analysis.cost cell = cost then
                        let expansion = solve cell in
                        let der = Expand {cell; reduction; expansion} in
                        raise (Derivation_found der)
                    );
                  assert false
            with Derivation_found der -> der
        in
        table.:(cell) <- result;
        result
    in
    solve
  ) in
  fun cell -> Lazy.force solve cell

(* [fuzz target_length cell] generates a derivation of [cell] aiming to have
   [target_length] terminals.
   The result is a pair [actual_length, derivation] where [actual_length] is the
   actual number of terminals.
*)
let rec fuzz size0 cell =
  let current_cost = Reach.Analysis.cost cell in
  assert (current_cost < max_int);
  let size = Int.max size0 current_cost in
  let node, i_pre, i_post = Reach.Cell.decode cell in
  match Reach.Tree.split node with
  | R (l, r) ->
    let candidates = ref [] in
    iter_sub_nodes i_pre i_post l r ~f:(fun cl ->
        let l_cost = Reach.Analysis.cost cl in
        if l_cost = max_int then
          fun _ -> ()
        else
          fun cr ->
            let r_cost = Reach.Analysis.cost cr in
            if r_cost < max_int && l_cost + r_cost <= size then
              push candidates ((cl, cr), 1.0)
      );
    if List.is_empty !candidates then
      iter_sub_nodes i_pre i_post l r ~f:(fun cl ->
          let l_cost = Reach.Analysis.cost cl in
          fun cr ->
            let r_cost = Reach.Analysis.cost cr in
            if l_cost < max_int && r_cost < max_int then
              push candidates ((cl, cr), 1.0)
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
    let left = fuzz (sl + mid) cl in
    (* Bias right side:
       - first set an expectation on the position of the split
         between left and right side,
       - generate left side targetting the split position
       - compensate on right side if left side missed expectation
    *)
    let right = fuzz (size - derivation_length left) cr in
    derivation_node cell left right
  | L tr ->
    match Transition.split grammar tr with
    | R shift ->
      (* We reached a shift transition *)
      let terminal = Transition.shift_symbol grammar shift in
      Shift {cell; terminal}
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
      if size <= 0 && nullable then
        Null {cell}
      else
        let candidates = ref (
            (* Compute weight of nullable case *)
            let weight =
              List.fold_left (fun acc reduction ->
                  if IndexSet.quick_subset c_post reduction.Reach.lookahead
                  then acc +. weights.:(reduction.production)
                  else acc
                ) 0.0 eqns.nullable
            in
            if weight > 0.0 then
              (* Weight again by the targeted size: avoid selecting null case
                 when a lot of tokens are expected *)
              [None, weight /. float (Int.max 1 size)]
            else
              []
          ) in
        (* Add recursive candidates *)
        iter_eqns i_pre i_post goto ~f:(fun reduction cell ->
            if Reach.Analysis.cost cell <= size then
              push candidates (Some (reduction, cell),
                               weights.:(reduction.production))
          );
        match sample_list !candidates with
        | None -> Null {cell}
        | Some (reduction, cell) ->
          let expansion = fuzz size cell in
          Expand {cell; expansion; reduction}

let () = Misc.stopwatch 1 "Start BFS"

let entrypoints =
  let accepting = Transition.accepting grammar in
  match !opt_entrypoints with
  | [] -> accepting
  | entrypoints ->
    let table = Hashtbl.create 7 in
    List.iter (fun name -> Hashtbl.replace table name ()) entrypoints;
    let entries =
      IndexSet.filter (fun tr ->
          let nt =
            Nonterminal.to_string grammar
              (Transition.goto_symbol grammar tr)
          in
          if Hashtbl.mem table nt
          then (Hashtbl.remove table nt; true)
          else false
        ) accepting
    in
    if Hashtbl.length table = 0 then
      entries
    else
      let keys = List.of_seq (Hashtbl.to_seq_keys table) in
      let cache = Damerau_levenshtein.make_cache () in
      let prepare_candidate tr =
        let text =
          Nonterminal.to_string grammar
            (Transition.goto_symbol grammar tr)
        in
        let distance =
          List.fold_left (fun acc key ->
              Int.min acc (Damerau_levenshtein.distance cache key text))
            max_int keys
        in
        (distance, text)
      in
      let candidates =
        IndexSet.diff accepting entries
        |> IndexSet.elements
        |> List.map prepare_candidate
        |> List.sort (compare_fst Int.compare)
      in
      Syntax.error Lexing.dummy_pos
        "unknown entrypoint%s %s.\n\
         valid entrypoints are:\n  %s."
        (if List.compare_length_with keys 1 > 0 then "s" else "")
        (String.concat ", " entrypoints)
        (string_concat_map ", " snd candidates)

type 'a zipper_path =
  | Left_of of {right: 'a; cell: Reach.Cell.n index}
  | Right_of of {left: 'a; cell: Reach.Cell.n index}
  | In_expansion of {reduction: Reach.reduction; cell: Reach.Cell.n index}

let bfs = Vector.make Reach.Cell.n []

let () =
  let todo = ref [] in
  let reachable cell = Reach.Analysis.cost cell < max_int in
  let visit path cell =
    match bfs.:(cell) with
    | _ :: _ -> ()
    | [] ->
      bfs.:(cell) <- path;
      push todo cell
  in
  let propagate cell =
    let path = bfs.:(cell) in
    let node, i_pre, i_post = Reach.Cell.decode cell in
    match Reach.Tree.split node with
    | R (l, r) ->
      iter_sub_nodes ~f:(fun left right ->
          if reachable left && reachable right then (
            visit (Left_of {right; cell} :: path) left;
            visit (Right_of {left; cell} :: path) right;
          )
        ) i_pre i_post l r
    | L tr ->
      match Transition.split grammar tr with
      | R _ -> ()
      | L gt ->
        iter_eqns i_pre i_post gt ~f:(fun reduction cell' ->
            if weights.:(reduction.production) > 0.0 then
              visit (In_expansion {reduction; cell} :: path) cell'
          )
  in
  IndexSet.iter (fun tr ->
      let node = Reach.Tree.leaf (Transition.of_goto grammar tr) in
      visit [] (Reach.Cell.encode node ~pre:0 ~post:0)
    ) entrypoints;
  let counter = ref 0 in
  fixpoint ~counter ~propagate todo;
  Misc.stopwatch 1 "Stop BFS (depth: %d)" !counter

(* Check we know how to print each terminal *)

let terminal_text = Token_printer.for_grammar grammar []

let _terminals_of_derivation der =
  let rec loop acc = function
    | Null _ -> acc
    | Shift  {terminal; _} -> terminal :: acc
    | Node   {left; right; _} -> loop (loop acc right) left
    | Expand {expansion; _} -> loop acc expansion
  in
  loop [] der

let rec iter_terminals_of_derivation ~f = function
  | Null _ -> ()
  | Shift  {terminal; _} -> f terminal
  | Node   {left; right; _} ->
    iter_terminals_of_derivation ~f left;
    iter_terminals_of_derivation ~f right
  | Expand {expansion; _} ->
    iter_terminals_of_derivation ~f expansion

let generate_sentence ?(length=100) ?from () =
  let tr = match from with
    | None -> IndexSet.choose (Transition.accepting grammar)
    | Some tr -> tr
  in
  let tr = Transition.of_goto grammar tr in
  assert (Array.length (Reach.Classes.pre_transition tr) = 1);
  assert (Array.length (Reach.Classes.post_transition tr) = 1);
  let node = Reach.Tree.leaf tr in
  let cell = Reach.Cell.encode node ~pre:0 ~post:0 in
  fuzz length cell

let output_with_comments oc =
  let count = ref 0 in
  fun t ->
    if !count > 0 then output_char oc ' ';
    Printf.fprintf oc "(* C%d *) %s" !count terminal_text.:(t);
    incr count

let directly_output oc =
  let need_sep = ref false in
  fun t ->
    if !need_sep then output_char oc ' ';
    need_sep := true;
    output_string oc terminal_text.:(t)

let () =
  let output_terminal () =
    if !opt_comments
    then output_with_comments stdout
    else directly_output stdout
  in
  let entrypoints =
    IndexSet.elements entrypoints
    |> List.map (fun tr -> tr, 1.0)
  in
  try
    match List.rev !opt_focus with
    | [] when not !opt_exhaust ->
      for _ = 0 to !opt_count - 1 do
        let derivation =
          generate_sentence
            ~from:(sample_list entrypoints)
            ~length:!opt_length ()
        in
        iter_terminals_of_derivation derivation ~f:(output_terminal ());
        output_char stdout '\n'
      done
    | focus ->
      let todo = Boolvector.make Reach.Cell.n !opt_exhaust in
      if not !opt_exhaust then (
        let focused_sym = Boolvector.make (Symbol.cardinal grammar) false in
        let focused_prods = Boolvector.make (Production.cardinal grammar) false in
        let focused_items = Vector.make (Production.cardinal grammar) IntSet.empty in
        let process_focus filter =
          match transl_filter filter with
          | Either.Left sym ->
            Boolvector.set focused_sym sym
          | Either.Right prods ->
            List.iter (fun (prod, dots) ->
                if IntSet.is_empty dots
                then Boolvector.set focused_prods prod
                else focused_items.@(prod) <- IntSet.union dots
              ) prods
        in
        List.iter (fun spec -> process_focus (parse_pattern spec)) focus;
        let set_node node = Reach.Cell.iter_node node (Boolvector.set todo) in
        Index.iter (Transition.any grammar) (fun tr ->
            if Boolvector.test focused_sym (Transition.symbol grammar tr) then
              set_node (Reach.Tree.leaf tr)
          );
        let rec visit_items i node f =
          f node i;
          let i =
            match Reach.Tree.split node with
            | L _ -> i + 1
            | R (l, r) ->
              let i = visit_items i l f in
              let i = visit_items i r f in
              i
          in
          f node i;
          i
        in
        Index.iter (Transition.goto grammar) (fun gt ->
            let eqns = Reach.Tree.goto_equations gt in
            if List.exists
                (fun {Reach.production; _} -> Boolvector.test focused_prods production)
                eqns.nullable then
              set_node (Reach.Tree.leaf (Transition.of_goto grammar gt));
            List.iter begin fun (red, node) ->
              if Boolvector.test focused_prods red.Reach.production then
                set_node node;
              let dots = focused_items.:(red.Reach.production) in
              if not (IntSet.is_empty dots) then
                ignore (visit_items 0 node (fun node i ->
                    if IntSet.mem i dots then
                      set_node node
                  ) : int)
            end eqns.non_nullable;
          );
      );
      let rec mark_derivation der =
        Boolvector.clear todo (derivation_cell der);
        derivation_iter_sub mark_derivation der
      in
      let gen_cell length cell =
        let der =
          if !opt_exhaust
          then min_sentence cell
          else fuzz length cell
        in
        mark_derivation der;
        der
      in
      let unroll_path der = function
        | Left_of {right; cell} ->
          derivation_node cell der right
        | Right_of {left; cell} ->
          derivation_node cell left der
        | In_expansion {reduction; cell} ->
          Expand {expansion = der; reduction; cell}
      in
      Index.iter Reach.Cell.n (fun cell ->
          if Reach.Analysis.cost cell < max_int && Boolvector.test todo cell then (
            match bfs.:(cell) with
            | [] -> () (* Unreachable *)
            | path ->
              let length = ref !opt_length in
              let path = List.map (function
                  | Left_of {right; cell} ->
                    let right = gen_cell 0 right in
                    length := !length - derivation_length right;
                    Left_of {right; cell}
                  | Right_of {left; cell} ->
                    let left = gen_cell 0 left in
                    length := !length - derivation_length left;
                    Right_of {left; cell}
                  | In_expansion _ as x -> x
                ) path
              in
              let der = gen_cell !length cell in
              let der = List.fold_left unroll_path der path in
              if !opt_print_entrypoint then (
                let entrypoint =
                  match List.fold_left (fun _ p -> p) (List.hd path) path with
                  | In_expansion {cell; _} ->
                    let node, _, _ = Reach.Cell.decode cell in
                    begin match Reach.Tree.split node with
                      | R _ -> assert false
                      | L tr -> Transition.symbol grammar tr
                    end
                  | _ -> assert false
                in
                output_string stdout (Symbol.name grammar entrypoint);
                output_string stdout ": ";
              );
              iter_terminals_of_derivation ~f:(output_terminal ()) der;
              output_char stdout '\n'
          )
        )
  with
  | Error (pos, msg) ->
    Syntax.error pos "%s." msg
  | Transl.Unknown_symbol (pos, name) ->
    let candidates = ref [] in
    let cache = Damerau_levenshtein.make_cache () in
    Index.iter (Symbol.cardinal grammar) (fun sym ->
        let name' = Symbol.name grammar sym in
        let dist = Damerau_levenshtein.distance cache name name' in
        if dist <= 7 then
          push candidates (dist, name')
      );
    match
      List.sort (compare_fst Int.compare) !candidates
      |> List.take 10
      |> List.rev_map snd
    with
    | [] -> Syntax.error pos "unknown symbol %s." name
    | [x] -> Syntax.error pos "unknown symbol %s.\nDid you mean %s?" name x
    | x :: xs ->
      Syntax.error pos "unknown symbol %s.\nDid you mean %s?" name
        (String.concat ", " (List.rev (("or " ^ x) :: xs)))
