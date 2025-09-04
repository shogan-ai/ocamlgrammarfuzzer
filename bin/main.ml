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
let opt_avoid = ref ["error"]
let opt_focus = ref []
let opt_exhaust = ref false
let opt_ocamlformat_check = ref false
let opt_ocamlformat = ref "ocamlformat"
let opt_max_errors_report = ref 20
let opt_jobs = ref 8
let opt_batch_size = ref 80

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
  ("--ocamlformat", Arg.Set_string opt_ocamlformat, " Path to OCamlformat command to use");
  ("--ocamlformat-check", Arg.Set opt_ocamlformat_check, " Check generated sentences with ocamlformat (default is to print them)");
  ("--max-report", Arg.Set_int opt_max_errors_report, " Maximum number of derivations to report per error (default: 20)");
  ("--jobs", Arg.Set_int opt_jobs, " Number of ocamlformat processes to run in parallel (default: 8)");
  ("--batch-size", Arg.Set_int opt_batch_size, " Number of files to submit to each ocamlformat process (default: 80)");
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
      then Oxcaml_grammar.content
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
    Reachability.Coercion.infix (Reach.Tree.post_classes l)
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
  end coercion.Reachability.Coercion.forward

let iter_eqns ~f i_pre i_post goto =
  let tr = Transition.of_goto grammar goto in
  let c_pre = (Reach.Classes.pre_transition tr).(i_pre) in
  let c_post = (Reach.Classes.post_transition tr).(i_post) in
  let eqns = Reach.Tree.goto_equations goto in
  List.iter begin fun ((reduction : _ Reachability.reduction), node') ->
    if IndexSet.disjoint c_post reduction.lookahead then
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

exception Derivation_found of (g, Reach.r, Reach.r Reachability.cell index) Derivation.t

let min_sentence =
  let solve = lazy (
    let sentinel = Derivation.null (Index.of_int Reach.Cell.n 0) in
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
            Derivation.null cell
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
                          Derivation.node cell (solve cl) (solve cr)
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
                  Derivation.shift cell terminal
                | L goto ->
                  iter_eqns i_pre i_post goto ~f:begin fun reduction cell ->
                    if Reach.Analysis.cost cell = cost then
                      let expansion = solve cell in
                      let der = Derivation.expand cell expansion reduction in
                      raise (Derivation_found der)
                  end;
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
    let right = fuzz (size - Derivation.length left) cr in
    Derivation.node cell left right
  | L tr ->
    match Transition.split grammar tr with
    | R shift ->
      (* We reached a shift transition *)
      Derivation.shift cell (Transition.shift_symbol grammar shift)
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
        Derivation.null cell
      else
        let candidates = ref (
            (* Compute weight of nullable case *)
            let weight =
              List.fold_left (fun acc (reduction : _ Reachability.reduction) ->
                  if IndexSet.quick_subset c_post reduction.lookahead
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
        | None -> Derivation.null cell
        | Some (reduction, cell) ->
          Derivation.expand cell (fuzz size cell) reduction

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
            visit (Derivation.Left_of {right; meta=cell} :: path) left;
            visit (Derivation.Right_of {left; meta=cell} :: path) right;
          )
        ) i_pre i_post l r
    | L tr ->
      match Transition.split grammar tr with
      | R _ -> ()
      | L gt ->
        iter_eqns i_pre i_post gt ~f:(fun reduction cell' ->
            if weights.:(reduction.production) > 0.0 then
              visit (In_expansion {reduction; meta=cell} :: path) cell'
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

let derivations =
  let entrypoints =
    IndexSet.elements entrypoints
    |> List.map (fun tr -> tr, 1.0)
  in
  match List.rev !opt_focus with
  | [] when not !opt_exhaust ->
    Seq.init !opt_count (fun _ ->
        generate_sentence
          ~from:(sample_list entrypoints)
          ~length:!opt_length ()
      )
  | focus ->
    let todo = Boolvector.make Reach.Cell.n !opt_exhaust in
    if not !opt_exhaust then (
      try
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
                (fun {Reachability.production; _} ->
                   Boolvector.test focused_prods production)
                eqns.nullable then
              set_node (Reach.Tree.leaf (Transition.of_goto grammar gt));
            List.iter begin fun ((red : _ Reachability.reduction), node) ->
              if Boolvector.test focused_prods red.production then
                set_node node;
              let dots = focused_items.:(red.production) in
              if not (IntSet.is_empty dots) then
                ignore (visit_items 0 node (fun node i ->
                    if IntSet.mem i dots then
                      set_node node
                  ) : int)
            end eqns.non_nullable;
          );
      with
      | Error (pos, msg) ->
        Syntax.error pos "%s." msg
      | Transl.Unknown_symbol (pos, name) ->
        let candidates = ref [] in
        let cache = Damerau_levenshtein.make_cache () in
        Index.iter (Symbol.cardinal grammar) begin fun sym ->
          let name' = Symbol.name grammar sym in
          let dist = Damerau_levenshtein.distance cache name name' in
          if dist <= 7 then
            push candidates (dist, name')
        end;
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
    );
    let rec mark_derivation der =
      Boolvector.clear todo (Derivation.meta der);
      Derivation.iter_sub mark_derivation der
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
    let enum = Index.enumerate Reach.Cell.n in
    let rec next_cell () =
      match enum () with
      | exception Index.End_of_set -> Seq.Nil
      | cell when Reach.Analysis.cost cell = max_int (* empty language *) ||
                  not (Boolvector.test todo cell) ||
                  List.is_empty bfs.:(cell) (* unreachable from entrypoint *)
        -> next_cell ()
      | cell ->
        let length = ref !opt_length in
        let gen_path_component cell =
          let der = gen_cell 0 cell in
          length := !length - Derivation.length der;
          der
        in
        let path = List.map (Derivation.map_path gen_path_component) bfs.:(cell) in
        let leaf = gen_cell !length cell in
        let der = List.fold_left Derivation.unroll_path leaf path in
        Seq.Cons (der, next_cell)
    in
    next_cell

let report_invalid_entrypoint =
  let reported = ref IndexSet.empty in
  fun ep ->
  if not (IndexSet.mem ep !reported) then (
    reported := IndexSet.add ep !reported;
    true
  ) else
    false

let entrypoint_of_derivation der =
    let node, _, _ = Reach.Cell.decode (Derivation.meta der) in
    match Reach.Tree.split node with
    | R _ -> assert false
    | L tr -> Transition.symbol grammar tr

let prepare_derivation_for_check ~with_comments der =
  let buf = Buffer.create 31 in
  let comments = ref 0 in
  let add_comment () =
    if Buffer.length buf > 0 then
      Buffer.add_char buf ' ';
    Printf.bprintf buf "(* C%d *)" !comments;
    incr comments
  in
  let locations = ref [] in
  let add_terminal terminal =
    match terminal_text.:(terminal) with
    | "" -> () (* ignore EOF *)
    | text ->
      if with_comments then add_comment ();
      let startp = match Buffer.length buf with
        | 0 -> 0
        | n -> Buffer.add_char buf ' '; (n + 1)
      in
      Buffer.add_string buf text;
      let endp = Buffer.length buf in
      push locations (startp, endp)
  in
  Derivation.iter_terminals ~f:add_terminal der;
  if with_comments then add_comment ();
  let locations =
    Array.of_list (List.rev !locations)
  in
  let kind =
    let entrypoint = entrypoint_of_derivation der in
    match Symbol.name grammar entrypoint with
    | "implementation" -> `Impl
    | "interface" -> `Intf
    | name ->
      if report_invalid_entrypoint entrypoint then
        Syntax.warn Lexing.dummy_pos
          "ocamlformat-check: invalid entrypoint %s, \
           only implementation and interface are supported"
          name;
      `Impl
  in
  (kind, locations, Buffer.contents buf)

let find_erroneous_token locations (startp, endp) =
  let tok_loc (startp', _ : int * int) = startp' >= startp in
  match Array.find_index tok_loc locations.tokens with
  | Some i ->
    let startp', endp' = locations.tokens.(i) in
    if startp = startp' then
      if endp = endp' then
        Some i
      else if endp = endp' - 1 then (
        Printf.eprintf "off-by-one in token location: error at %d-%d, token at %d-%d\n"
          startp endp startp' endp';
        None
      ) else (
        Printf.eprintf "invalid token location: error at %d-%d, token at %d-%d\n"
          startp endp startp' endp';
        None
      )
    else if i < Array.length locations.comments then
      let startp', endp' = locations.comments.(i) in
      if startp = startp' then
        Some i
      else if endp = endp' - 1 then (
        Printf.eprintf "off-by-one in comment location: error at %d-%d, token at %d-%d\n"
          startp endp startp' endp';
        None
      ) else (
        Printf.eprintf "invalid comment location: error at %d-%d, token at %d-%d\n"
          startp endp startp' endp';
        None
      )
    else (
      Printf.eprintf "invalid error location: error at %d-%d, closest token at %d-%d\n"
        startp endp startp' endp';
      None
    )
  | None -> None

module Occurence_heap : sig
  type ('n, 'a) t
  val make : 'n cardinal -> ('n, 'a) t
  val add : ('n, 'a) t -> 'n indexset -> 'a -> unit
  val pop : ('n, 'a) t -> ('n index * 'a list) option
  val pop_seq : ('n, 'a) t -> ('n index * 'a list) Seq.t
end = struct

  type ('n, 'a) elt = {
    payload: 'a;
    mutable active: bool;
    occurrences: 'n indexset;
  }

  type ('n, 'a) t = {
    table: ('n, ('n, 'a) elt list) vector;
    outdated: 'n Boolvector.t;
    mutable todo : 'n indexset;
    mutable ranks : 'n indexset IntMap.t;
  }

  let make n = {
    table = Vector.make n [];
    outdated = Boolvector.make n false;
    todo = IndexSet.empty;
    ranks = IntMap.empty;
  }

  let mark_todo t occ =
    if not (Boolvector.test t.outdated occ) then (
      begin match List.length t.table.:(occ) with
        | 0 -> ()
        | count ->
          t.ranks <- IntMap.update count (function
              | None -> assert false
              | Some set ->
                let set' = IndexSet.remove occ set in
                assert (set != set');
                if IndexSet.is_empty set'
                then None
                else Some set'
            ) t.ranks
      end;
      Boolvector.set t.outdated occ;
      t.todo <- IndexSet.add occ t.todo
    )

  let add t occurrences payload =
    let elt = {payload; occurrences; active = true} in
    IndexSet.iter
      (fun occ -> mark_todo t occ; t.table.@(occ) <- List.cons elt)
      occurrences

  let reindex t =
    let todo = t.todo in
    t.todo <- IndexSet.empty;
    IndexSet.iter begin fun occ ->
      Boolvector.clear t.outdated occ;
      let elts = Misc.list_rev_filter (fun elt -> elt.active) t.table.:(occ) in
      t.table.:(occ) <- elts;
      match List.length elts with
      | 0 -> ()
      | count ->
        t.ranks <- IntMap.update count (function
            | None -> Some (IndexSet.singleton occ)
            | Some set -> Some (IndexSet.add occ set)
          ) t.ranks
    end todo

  let pop t =
    reindex t;
    match IntMap.max_binding_opt t.ranks with
    | None -> None
    | Some (_, occs) ->
      let occ = IndexSet.choose occs in
      let elts =
        List.rev_map (fun elt ->
            assert elt.active;
            elt.active <- false;
            IndexSet.iter (mark_todo t) elt.occurrences;
            elt.payload
          ) t.table.:(occ)
      in
      Some (occ, elts)

  let pop_seq t =
    let rec self () =
      match pop t with
      | None -> Seq.Nil
      | Some x ->
        Seq.Cons (x, self)
    in
    self
end

let _ = Occurence_heap.pop

type 'a error = {
  path: g item index list;
  derivation: (g, Reach.r, g item index list *
                           Reach.Cell.n index *
                           g item index list) Derivation.t;
  error: 'a;
}

(* Some messages contain a comment.
   They are noise when trying to classify problems, so let's remove them. *)

(*let cleanup_comment str =
  let l = String.length str in
  let b = Buffer.create l in
  let i = ref 0 in
  while !i < l - 6 do
    let i0 = !i in
    let c = str.[i0] in
    if c = '(' && str.[i0 + 1] = '*' then (
      i := i0 + 2;

      while !i < l - 3 && str.[!i] = ' ' do
        incr i
      done;

      if str.[!i] = 'C' then (
        incr i;
        while !i < l - 2 && let c = str.[!i] in c >= '0' && c <= '9' do
          incr i
        done;
        while !i < l - 2 && str.[!i] = ' ' do
          incr i
        done;
        if str.[!i] = '*' && str.[!i+1] = ')' then (
          i := !i + 2;
          Buffer.add_string b "(* ... *)"
        ) else (
          Buffer.add_substring b str i0 (!i - i0);
        )
      ) else (
        Buffer.add_substring b str i0 (!i - i0)
      )
    ) else (
      Buffer.add_char b c;
      incr i
    )
  done;
  Buffer.add_substring b str !i (l - !i);
  Buffer.contents b

let prepare_message lines =
  let rec split_code_delimiter = function
    | [] -> lines
    | x :: xs ->
      let found = ref false in
      if String.for_all (fun c ->
          if c = '^' then (found := true; true)
          else if c = ' ' then not !found
          else false
        ) x
      then xs
      else
        split_code_delimiter xs
  in
  List.map cleanup_comment (split_code_delimiter lines) *)

let plural = function
  | [] | [_] -> ""
  | _ -> "s"

let report_error_samples ~with_comment errors =
  let exception Exit_iter in
  let iter_by ~compare xs f =
    try
      xs
      |> group_by ~compare ~group:(fun x xs -> (1 + List.length xs, x, xs))
      |> List.sort (fun (r1, _, _) (r2, _, _) -> Int.compare r2 r1)
      |> List.iteri (fun i (_, x, xs) -> f i x xs)
    with Exit_iter -> ()
  in
  let compare_path e1 e2 =
    List.compare Index.compare e1.path e2.path
  in
  iter_by errors ~compare:compare_path begin fun i (e : ([`Intf | `Impl] * int option) error) es ->
    if i > !opt_max_errors_report then (
      Printf.printf "- ...\n";
      raise Exit_iter
    );
    Printf.printf "- Derivation (%d occurrence%s):\n"
      (1 + List.length es)
      (plural (e :: es));
    Printf.printf "  ```\n";
    List.iteri begin fun i x ->
      Printf.printf "  %s%s\n"
        (String.make (2 * i) ' ')
        (Item.to_string grammar x);
    end (List.rev e.path);
    Printf.printf "  ```\n";
    let sample = List.fold_left
        (fun e e' ->
           if Derivation.length e'.derivation <
              Derivation.length e.derivation
           then e'
           else e
        ) e es
    in
    let kind, position = sample.error in
    Printf.printf "  Sample sentence (%s):\n"
      (match kind with `Impl -> "implementation" | `Intf -> "interface");
    Printf.printf "  ```ocaml\n";
    let i = ref 0 in
    let buf = Buffer.create 31 in
    let hilight = ref (0,0) in
    let add_text text =
      let startp = match Buffer.length buf with
        | 0 -> 0
        | n -> Buffer.add_char buf ' '; n + 1
      in
      Buffer.add_string buf text;
      if Some !i = position then
        hilight := (startp, Buffer.length buf);
      incr i
    in
    Derivation.iter_terminals sample.derivation
      ~f:(fun t ->
          if Some !i = position && with_comment then
            add_text "(* ... *)";
          add_text terminal_text.:(t);
        );
    Printf.printf "  %s\n" (Buffer.contents buf);
    begin match !hilight with
      | (s, e) when e > s ->
        Printf.printf "  %s%s\n" (String.make s ' ') (String.make (e-s) '^')
      | _ -> ()
    end;
    Printf.printf "  ```\n";
  end;
  Printf.printf "\n"

let report_syntax_errors derivations sources outcome =
  Printf.printf "# Syntax errors\n\n";
  (* Filter and group by error message *)
  let by_message = Hashtbl.create 7 in
  for i = 0 to Array.length derivations - 1 do
    let kind, locations, text = sources.(i) in
    List.iter begin function
      | Ocamlformat.Error.Internal _
      | Comment_dropped _ -> ()
      | Syntax error when error.line <> 1 ->
        Printf.eprintf "Internal error: unexpected error at %d.%d-%d (sentence: %S)\n"
          error.line error.start_col error.end_col text
      | Syntax error ->
        match find_erroneous_token locations (error.start_col, error.end_col) with
        | None ->
          Printf.eprintf "Invalid error report: unexpected error at %d.%d-%d (sentence: %S)\n"
            error.line error.start_col error.end_col text
        | Some pos ->
          let expansion, reduction =
            match derivations.(i).Derivation.desc with
            | Expand {expansion; reduction} -> (expansion, reduction)
            | _ -> assert false
          in
          let derivation = Derivation.items_of_expansion grammar ~expansion ~reduction in
          let path, _cell, _path = Derivation.get_meta derivation pos in
          let message = error.message in
          let error = {derivation; error = (kind, Some pos); path} in
          match Hashtbl.find_opt by_message message with
          | None -> Hashtbl.add by_message message (ref [error])
          | Some r -> push r error
    end outcome.(i)
  done;
  (* Order by number of occurrences *)
  let by_message =
    Array.of_seq (Seq.map (fun (k, v) -> (k, List.length !v, !v))
                    (Hashtbl.to_seq by_message))
  in
  Array.sort (fun (_,r1,_) (_,r2,_) -> Int.compare r2 r1) by_message;
  (* Classify by item *)
  let heap = Occurence_heap.make (Item.cardinal grammar) in
  Array.iter (fun (message, _, errors) ->
      Printf.printf "## %s\n" message;
      (* Errors by most frequent items *)
      List.iter
        (fun e -> Occurence_heap.add heap (IndexSet.of_list e.path) e)
        errors;
      Seq.iter (fun (item, errors) ->
          Printf.printf "\n### Item `%s` (in %d error%s)\n\n"
            (Item.to_string grammar item)
            (List.length errors) (plural errors);
          report_error_samples ~with_comment:false errors;
        ) (Occurence_heap.pop_seq heap)
    ) by_message;
  Printf.printf "\n"

let report_comment_dropped derivations sources outcome =
  Printf.printf "# Comment dropped\n\n";
  (* Classify by item *)
  let heap = Occurence_heap.make (Item.cardinal grammar) in
  for i = 0 to Array.length derivations - 1 do
    let kind, _locations, _text = sources.(i) in
    List.iter begin function
      | Ocamlformat.Error.Internal _ | Syntax _ -> ()
      | Comment_dropped pos ->
        let expansion, reduction =
          match derivations.(i).Derivation.desc with
          | Expand {expansion; reduction} -> (expansion, reduction)
          | _ -> assert false
        in
        let derivation = Derivation.items_of_expansion grammar ~expansion ~reduction in
        let path, _cell, _path = Derivation.get_meta derivation pos in
        Occurence_heap.add heap (IndexSet.of_list path)
          {derivation; error = (kind, Some pos); path}
    end outcome.(i)
  done;
  (* Print errors by most frequent item *)
  Seq.iter (fun (item, errors) ->
      Printf.printf "## Item `%s` (in %d error%s)\n"
        (Item.to_string grammar item)
        (List.length errors) (plural errors);
      report_error_samples ~with_comment:true errors;
    ) (Occurence_heap.pop_seq heap);
  Printf.printf "\n"

let report_internal_errors derivations sources outcome =
  Printf.printf "# Internal errors\n\n";
  Printf.printf "Note: when OCamlformat fails with an internal error, \
                 it is not possible to know the location of the problem.\n";
  Printf.printf
    "Locations for these errors are guessed by looking at the syntactic \
     constructions that appear most often in the failing sentences.\n\n";

  (* Filter and group by error message *)
  let by_message = Hashtbl.create 7 in
  let safe_cells = Boolvector.make Reach.Cell.n false in
  for i = 0 to Array.length derivations - 1 do
    match outcome.(i) with
    | [] ->

      (* No error, mark all cells in this derivation as safe *)
      let rec mark_safe der =
        Boolvector.set safe_cells (Derivation.meta der);
        Derivation.iter_sub mark_safe der
      in
      mark_safe derivations.(i)

    | errors ->
      let kind, _locations, _text = sources.(i) in
      List.iter begin function
        | Ocamlformat.Error.Syntax _
        | Comment_dropped _ -> ()
        | Internal message ->
          let expansion, reduction =
            match derivations.(i).Derivation.desc with
            | Expand {expansion; reduction} -> (expansion, reduction)
            | _ -> assert false
          in
          let derivation = Derivation.items_of_expansion grammar ~expansion ~reduction in
          let unsafe_items = ref IndexSet.empty in
          let add_item item = unsafe_items := IndexSet.add item !unsafe_items in
          let rec collect_unsafe der =
            let path, cell, _path = Derivation.meta der in
            if not (Boolvector.test safe_cells cell) then
              List.iter add_item path;
            Derivation.iter_sub collect_unsafe der
          in
          collect_unsafe derivation;
          let rec collect_any der =
            let path, _cell, _path = Derivation.meta der in
            List.iter add_item path;
            Derivation.iter_sub collect_any der
          in
          if IndexSet.is_empty !unsafe_items then
            collect_any derivation;
          assert (not (IndexSet.is_empty !unsafe_items));
          let error = {derivation; error = (kind, !unsafe_items); path = []} in
          match Hashtbl.find_opt by_message message with
          | None -> Hashtbl.add by_message message (ref [error])
          | Some r -> push r error
      end errors
  done;
  let annotate_with_item item error =
    let path = ref [] in
    let pos = ref 0 in
    let find_pos any der =
      let rec loop der =
        let pos' = !pos in
        Derivation.iter_sub loop der;
        let path', cell, _ = Derivation.meta der in
        if (any || not (Boolvector.test safe_cells cell)) &&
           List.mem item path'
        then (
          pos := pos';
          path := path';
          raise Exit
        );
        match der.desc with
        | Shift _ -> incr pos
        | _ -> ()
      in
      pos := 0;
      loop der
    in
    begin
      try
        find_pos false error.derivation;
        find_pos true error.derivation
      with Exit -> ()
    end;
    let kind, _ = error.error in
    {error with error = (kind, Some !pos); path = !path}
  in
  (* Order by number of occurrences *)
  let by_message =
    Array.of_seq (Seq.map (fun (k, v) -> (k, List.length !v, !v))
                    (Hashtbl.to_seq by_message))
  in
  Array.sort (fun (_,r1,_) (_,r2,_) -> Int.compare r2 r1) by_message;
  (* Classify by item *)
  let heap = Occurence_heap.make (Item.cardinal grammar) in
  Array.iter (fun (message, _, errors) ->
      Printf.printf "## %s (%d error%s)\n" message (List.length errors) (plural errors);
      (* Errors by most frequent items *)
      List.iter
        (fun e -> Occurence_heap.add heap (snd e.error) e)
        errors;
      Seq.iter (fun (item, errors) ->
          Printf.printf "\n### Item `%s` (in %d error%s)\n\n"
            (Item.to_string grammar item)
            (List.length errors) (plural errors);
          report_error_samples ~with_comment:false
            (List.map (annotate_with_item item) errors);
        ) (Occurence_heap.pop_seq heap)
    ) by_message;
  Printf.printf "\n"

let () =
  let output_with_comments oc =
    let count = ref 0 in
    fun t ->
      if !count > 0 then output_char oc ' ';
      Printf.fprintf oc "(* C%d *) %s" !count terminal_text.:(t);
      incr count
  in
  let directly_output oc =
    let need_sep = ref false in
    fun t ->
      if !need_sep then output_char oc ' ';
      need_sep := true;
      output_string oc terminal_text.:(t)
  in
  if not !opt_ocamlformat_check then (
    let output_terminal () =
      if !opt_comments
      then output_with_comments stdout
      else directly_output stdout
    in
    Seq.iter begin fun der ->
      if !opt_print_entrypoint then (
        let entrypoint =
          let node, _, _ = Reach.Cell.decode (Derivation.meta der) in
          match Reach.Tree.split node with
          | R _ -> assert false
          | L tr -> Transition.symbol grammar tr
        in
        output_string stdout (Symbol.name grammar entrypoint);
        output_string stdout ": ";
      );
      Derivation.iter_terminals ~f:(output_terminal ()) der;
      output_char stdout '\n'
    end derivations
  ) else (
    let derivations = Array.of_seq derivations in
    let sources =
      Array.map (prepare_derivation_for_check ~with_comments:!opt_comments) derivations
    in
    let outcome =
      Array.to_seq sources
      |> Seq.map (fun (k,_,s)  -> (k, s))
      |> Ocamlformat.check
        ~ocamlformat_command:!opt_ocamlformat
        ~jobs:(Int.max 0 !opt_jobs)
        ~batch_size:(Int.max 1 !opt_batch_size)
      |> Array.of_seq
    in
    let valid = ref 0 in
    let syntax_errors = ref 0 in
    let with_comments_dropped = ref 0 in
    let comments_dropped = ref 0 in
    let internal_errors = ref 0 in
    Array.iter (function
        | [] -> incr valid;
        | errors ->
          let had_comments_dropped = ref false in
          List.iter (function
              | Ocamlformat.Error.Internal _ ->
                incr internal_errors
              | Syntax _ ->
                incr syntax_errors
              | Comment_dropped _ ->
                incr comments_dropped;
                if not !had_comments_dropped then (
                  incr with_comments_dropped;
                  had_comments_dropped := true;
                )
            ) errors
      ) outcome;
    let count = Array.length outcome in
    let percent x = 100.0 *. float x /. float count in
    Printf.eprintf "Tested %d sentences:\n\
                    - %d successfully formated (%.02f%%)\n\
                    - %d failed with syntax errors (%.02f%%)\n\
                    - %d had comments dropped (%.02f%%) (%d comments were dropped in total)\n\
                    - %d caused internal errors (%.02f%%)\n%!"
      count
      !valid            (percent !valid)
      !syntax_errors    (percent !syntax_errors)
      !with_comments_dropped (percent !with_comments_dropped) !comments_dropped
      !internal_errors  (percent !internal_errors);
    report_syntax_errors derivations sources outcome;
    report_comment_dropped derivations sources outcome;
    report_internal_errors derivations sources outcome;
  )
