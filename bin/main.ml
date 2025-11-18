(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

[@@@warning "-33"]
open Fix.Indexing
open Utils
open Misc
open Grammarfuzzer

let opt_count = ref 0
let opt_length = ref 100
let opt_comments = ref false
let opt_seed = ref (-1)
let opt_oxcaml = ref false
let opt_lr1 = ref false
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
let opt_batch_size = ref 400
let opt_terminals = ref []
let opt_cmly = ref ""
let opt_print_derivations = ref []

let opt_save_report = ref "-"
let opt_save_successful = ref ""
let opt_save_lexer_errors = ref ""
let opt_save_parser_errors = ref ""
let opt_save_invariant_errors = ref ""
let opt_save_internal_errors = ref ""
let opt_save_comment_errors = ref ""

let opt_track_regressions_from = ref ""
let opt_track_regressions_to = ref ""
let opt_regressions_report_to = ref ""
let opt_regressions_non_fatal = ref false
let opt_debug_log_output = ref false

let add_terminal str =
  match String.split_on_char '=' str with
  | [key; value] -> push opt_terminals (key, value)
  | _ -> raise (Arg.Bad "Terminals should be specified using <name>=<text> syntax")

let spec_list = [
  (* Controlling generation *)
  ("--count"    , Arg.Set_int opt_count, "<int> Number of lines to generate"  );
  ("--length"   , Arg.Set_int opt_length, "<int> Number of token per sentence");
  ("--seed"     , Arg.Set_int opt_seed, "<int> Random seed");
  ("--entrypoint", Arg.String (push opt_entrypoints), "<symbol> Generate sentences from this entrypoint");
  ("--weight", Arg.String (push opt_weights), "<float> <pattern> Adjust the weights of grammatical constructions");
  ("--avoid", Arg.String (push opt_avoid), "<pattern> Forbid grammatical constructions");
  ("--focus", Arg.String (push opt_focus), "<pattern> Generate sentences stressing specific grammatical constructions");
  ("--exhaust", Arg.Set opt_exhaust, " Exhaust mode generates a deterministic set of sentences that cover all reachable constructions");
  ("--ocamlformat", Arg.Set_string opt_ocamlformat, "<path> OCamlformat command to use");
  ("--oxcaml"   , Arg.Set opt_oxcaml, " Work with Oxcaml dialect");
  ("--lr1"   , Arg.Set opt_lr1, " When using a builtin grammar (ocaml or --oxcaml), use LR(1) instead of LALR(1) automaton");
  ("--cmly", Arg.Set_string opt_cmly, "<path.cmly> Use grammar from the specified cmly file instead of builtin O(x)Caml grammar");
  (* Printer configuration *)
  ("--comments" , Arg.Set opt_comments , " Generate fake comments in the lines");
  ("--print-entrypoint", Arg.Set opt_print_entrypoint, " Prefix every sentence by the entrypoint followed by ':'");
  ("--terminal", Arg.String add_terminal, " Specify how a terminal should be printed; pass '--terminal INT=42' to print INT as '42'");
  (* Ocamlformat invocation setting *)
  ("--jobs", Arg.Set_int opt_jobs, "<int> Number of ocamlformat processes to run in parallel (default: 8)");
  ("--batch-size", Arg.Set_int opt_batch_size, "<int> Number of files to submit to each ocamlformat process (default: 400)");
  (* Check mode *)
  ("--ocamlformat-check"        , Arg.Set opt_ocamlformat_check, " Check mode: check generated sentences with ocamlformat (default is to print them)");
  ("--save-report-to"           , Arg.Set_string opt_save_report, "<path> In check mode, classify and report detected problems to a file (default to stdout)");
  ("--max-report"               , Arg.Set_int opt_max_errors_report, "<int> Maximum number of derivations to report per error (default: 20)");
  ("--save-successful-to"       , Arg.Set_string opt_save_successful, "<path> In check mode, save successful sentences to a file");
  ("--save-lexer-errors-to"     , Arg.Set_string opt_save_lexer_errors, "<path> In check mode, save lexer errors to a file");
  ("--save-parser-errors-to"    , Arg.Set_string opt_save_parser_errors, "<path> In check mode, save parser errors to a file");
  ("--save-invariant-errors-to" , Arg.Set_string opt_save_invariant_errors, "<path> In check mode, save invariant errors to a file");
  ("--save-comment-errors-to"   , Arg.Set_string opt_save_comment_errors, "<path> In check mode, save comment errors to a file");
  ("--save-internal-errors-to"  , Arg.Set_string opt_save_internal_errors, "<path> In check mode, save internal errors (including red herring) to a file");
  (* CI test *)
  ("--track-regressions-from"   , Arg.Set_string opt_track_regressions_from,
   "<path> In check mode, read success state from a previous run and compare them to current results. \
    If a regression is detected, exit code is set to 1."
  );
  ("--track-regressions-to"     , Arg.Set_string opt_track_regressions_to,
   "<path> In check mode, save success state in a file (in a custom text format).");
  ("--track-regressions-in"     , Arg.String (fun x -> opt_track_regressions_from := x; opt_track_regressions_to := x),
   "<path> --track-regressions-in x is --track-regressions-from x --track-regressions-to x"
  );
  ("--regressions-report-to"    , Arg.Set_string opt_regressions_report_to,
   "<path> In check mode and when tracking is enabled, report only the regressions (disabled by default)");
  ("--regressions-not-fatal"    , Arg.Set opt_regressions_non_fatal,
   " Exit code should not be affected by a regression.");
  (* Misc *)
  ("-v"         , Arg.Unit (fun () -> incr Misc.verbosity_level), " Increase verbosity");
  ("--print-derivation", Arg.String (push opt_print_derivations), "<sentence> Print the grammatical derivation of a sentence");
  ("--debug-log-output", Arg.Set opt_debug_log_output, " Log output of ocamlformat for debug purpose");
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

let cmly_content =
  if !opt_cmly <> "" then (
    if !opt_oxcaml || !opt_lr1 then
      prerr_endline "--cmly: a grammar has been provided, ignoring --oxcaml / --lr1";
    let ic = open_in_bin !opt_cmly in
    let length = in_channel_length ic in
    let data = really_input_string ic length in
    close_in ic;
    data
  ) else
    match !opt_oxcaml, !opt_lr1 with
    | true , false -> Oxcaml_grammar.lalr
    | true , true  -> Oxcaml_grammar.lr1
    | false, false -> Ocaml_grammar.lalr
    | false, true  -> Ocaml_grammar.lr1

module Grammar = MenhirSdk.Cmly_read.FromString(struct let content = cmly_content end)

include Info.Lift(Grammar)
open Info

(* Print requested derivations first (if any) *)

let interpreter =
  let find_symbol =
    let table = Hashtbl.create 7 in
    Index.iter (Symbol.cardinal grammar) (fun t ->
        Hashtbl.add table (Symbol.name grammar t) t;
      );
    fun name ->
      match Hashtbl.find_opt table name with
      | None ->
        Printf.eprintf "Unknown symbol: %s\n" name;
        exit 1
      | Some t -> t
  in
  let get_action =
    let table = Vector.make (Lr1.cardinal grammar) IndexMap.empty in
    let index lr1 =
      IndexMap.empty
      |> IndexSet.fold (fun tr map ->
          let target = Transition.target grammar tr in
          let sym = Option.get (Lr1.incoming grammar target) in
          IndexMap.add sym (`Shift target) map
        ) (Transition.successors grammar lr1)
      |> IndexSet.fold (fun red map ->
          let action = `Reduce (Reduction.production grammar red) in
          IndexSet.fold
            (fun term map -> IndexMap.add (Symbol.inj_t grammar term) action map)
            (Reduction.lookaheads grammar red)
            map
        ) (Reduction.from_lr1 grammar lr1)
    in
    let get_actions lr1 =
      match table.:(lr1) with
      | map when not (IndexMap.is_empty map) -> map
      | _ ->
        let map = index lr1 in
        table.:(lr1) <- map;
        map
    in
    fun lr1 sym ->
      match IndexMap.find_opt sym (get_actions lr1) with
      | None -> `Reject
      | Some action -> action
  in
  let rec split_at n acc = function
    | x :: xs when n > 0 -> split_at (n - 1) (x :: acc) xs
    | xs -> (List.rev acc, xs)
  in
  let split_at n xs = split_at n [] xs in
  let nodes_of_stack stack =
    List.rev_map
      (fun (lr1, child) ->
         let label = match Lr1.incoming grammar lr1 with
           | None ->
             let name =
               Nonterminal.to_string grammar
                 (Production.lhs grammar
                    (Option.get (Lr1.is_entrypoint grammar lr1)))
             in
             String.sub name 0 (String.length name - 1) ^ ":"
           | Some sym ->
             Symbol.name grammar sym
         in
         Derivation_printer.node label child)
      stack
  in
  let rec consume_symbol stack sym =
    match stack with
    | [] ->
      prerr_endline "Empty stack";
      None
    | (top, _) :: _ ->
      match get_action top sym with
      | `Reject ->
        Printf.eprintf "No action from state %s on symbol %s\n"
          (Lr1.to_string grammar top) (Symbol.name grammar sym);
        exit 1
      | `Shift state ->
        Some ((state, []) :: stack)
      | `Reduce prod ->
        let producers, stack = split_at (Production.length grammar prod) stack in
        let nodes = nodes_of_stack producers in
        let goto_sym = Symbol.inj_n grammar (Production.lhs grammar prod) in
        match get_action (fst (List.hd stack)) goto_sym with
        | `Reject | `Reduce _ -> failwith "Invalid automaton"
        | `Shift state ->
          consume_symbol ((state, nodes) :: stack) sym
  in
  fun entrypoint symbols ->
    let stack = [entrypoint, []] in
    let symbols = List.map find_symbol symbols in
    let rec loop stack = function
      | [] -> (stack, [])
      | sym :: rest as input ->
        match consume_symbol stack sym with
        | None -> (stack, input)
        | Some stack' -> loop stack' rest
    in
    let print_stack stack =
      let nodes = nodes_of_stack stack in
      Derivation_printer.output stdout nodes
    in
    let stack, remaining = loop stack symbols in
    if List.is_empty remaining then
      Printf.printf "Successful parse:\n"
    else
      Printf.printf "Input rejected after reaching:\n";
    print_stack stack;
    if not (List.is_empty remaining) then
      Printf.printf "Remaining symbols: %s\n"
        (string_concat_map ", " (Symbol.name grammar) remaining)

let parse_sentence text =
  let symbols = List.filter ((<>) "") (String.split_on_char ' ' text) in
  let entrypoint, symbols =
    match symbols with
    | entrypoint :: rest when entrypoint.[String.length entrypoint - 1] = ':' ->
      let entrypoint = String.sub entrypoint 0 (String.length entrypoint - 1) in
      begin match Hashtbl.find_opt (Lr1.entrypoint_table grammar) entrypoint with
      | None ->
        Printf.eprintf "Unknown entrypoint: %s\n" entrypoint;
        exit 1
      | Some ep -> ep, rest
      end
    | symbols -> IndexSet.choose (Lr1.entrypoints grammar), symbols
  in
  interpreter entrypoint symbols

let () =
  match List.rev !opt_print_derivations with
  | [] -> ()
  | inputs ->
    List.iter parse_sentence inputs;
    if List.is_empty !opt_focus && not !opt_exhaust && !opt_count = 0 then
      exit 0

(* Parse weight patterns *)

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
          let node, i_pre, i_post = Reach.Cell.decode cell in
          try
            begin match Reach.Tree.split node with
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
                )
            | L tr ->
              match Transition.split grammar tr with
              | R shift ->
                assert (cost = 1);
                (* We reached a shift transition *)
                let terminal = Transition.shift_symbol grammar shift in
                let der = Derivation.shift cell terminal in
                raise (Derivation_found der)
              | L goto ->
                iter_eqns i_pre i_post goto ~f:begin fun reduction cell' ->
                  if Reach.Analysis.cost cell' = cost then
                    let expansion = solve cell' in
                    let der = Derivation.expand cell expansion reduction in
                    raise (Derivation_found der)
                end
            end;
            assert (cost = 0);
            Derivation.null cell
          with Derivation_found der -> der
        in
        table.:(cell) <- result;
        assert (result.meta = cell);
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

let plural = function
  | [] | [_] -> ""
  | _ -> "s"

let entrypoints =
  let find_entrypoints entrypoints =
    let accepting = Transition.accepting grammar in
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
        (plural keys)
        (String.concat ", " entrypoints)
        (string_concat_map ", " snd candidates)
  in
  let entrypoints =
    match !opt_entrypoints with
    | [] -> ["implementation"; "interface"]
    | xs -> xs
  in
  find_entrypoints entrypoints

let bfs =
  let bfs = Vector.make Reach.Cell.n (max_int, []) in
  Misc.stopwatch 1 "Start BFS";
  let todo = ref [] in
  let visit cost path cell =
    match bfs.:(cell) with
    | (cost', _ :: _) when cost >= cost' -> ()
    | _ ->
      bfs.:(cell) <- (cost, path);
      push todo cell
  in
  let propagate cell =
    let (cost, path) = bfs.:(cell) in
    let node, i_pre, i_post = Reach.Cell.decode cell in
    match Reach.Tree.split node with
    | R (l, r) ->
      iter_sub_nodes ~f:(fun left right ->
          let cleft = Reach.Analysis.cost left in
          let cright = Reach.Analysis.cost right in
          if cleft < max_int && cright < max_int then (
            visit (cost + cright) (Derivation.Left_of {right; meta=cell} :: path) left;
            visit (cost + cleft) (Derivation.Right_of {left; meta=cell} :: path) right;
          )
        ) i_pre i_post l r
    | L tr ->
      match Transition.split grammar tr with
      | R _ -> ()
      | L gt ->
        iter_eqns i_pre i_post gt ~f:(fun reduction cell' ->
            if weights.:(reduction.production) > 0.0 then
              visit cost (In_expansion {reduction; meta=cell} :: path) cell'
          )
  in
  IndexSet.iter (fun tr ->
      let node = Reach.Tree.leaf (Transition.of_goto grammar tr) in
      visit 0 [] (Reach.Cell.encode node ~pre:0 ~post:0)
    ) entrypoints;
  let counter = ref 0 in
  fixpoint ~counter ~propagate todo;
  Misc.stopwatch 1 "Stop BFS (depth: %d)" !counter;
  bfs

(* Check we know how to print each terminal *)

let terminal_text = Token_printer.for_grammar grammar !opt_terminals

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
    let count = match !opt_count with
      | 0 when List.is_empty !opt_print_derivations -> 1
      | n -> n
    in
    Seq.init count (fun _ ->
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
          |> list_take 10
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
      der
    in
    let enum = Index.enumerate Reach.Cell.n in
    let rec next_cell () =
      match enum () with
      | exception Index.End_of_set -> Seq.Nil
      | cell when Reach.Analysis.cost cell = max_int (* empty language *) ||
                  not (Boolvector.test todo cell) ||
                  List.is_empty (snd bfs.:(cell)) (* unreachable from entrypoint *)
        -> next_cell ()
      | cell ->
        let length = ref !opt_length in
        let gen_path_component cell =
          let der = gen_cell 0 cell in
          length := !length - Derivation.length der;
          der
        in
        let path = List.map (Derivation.map_path gen_path_component) (snd bfs.:(cell)) in
        let leaf = gen_cell !length cell in
        let der = List.fold_left Derivation.unroll_path leaf path in
        mark_derivation der;
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

(* Code generation and location mapping.

   Looking at the locations reported by Ocamlformat can reveal a lot of information.
   We can distinguish many cases:
   - error location coincides exactly with a token   -> raw syntax error
   - error location coincides exactly with a comment -> comment error
   - location matches a subsequence of a token       -> lexer error
   - location matches exactly a range of tokens      -> syntax invariant error
   - location doesn't match the input                -> "red herring" error

   The last case, red herring, occurs when ocamlformat fails while looking for a
   fixed point: it successfully formats the input once, then try to process its
   own output and fail with an unexpected error that causes it to report a
   location referring to the formatted source, not to the input.

   It is a red herring because there are no direct way to tell that the location
   refers to another file, hidden from the end-user.

   A red herring error could also be missed if its location accidentally matches
   a valid input location.
   To avoid this case, we pad the inputs with many spaces to make it very
   unlikely for any formatted output to have overlapping locations.
*)
type error_kind =
  | Syntax
  | Comment
  | Lexer
  | Syntactic_invariant
  | Red_herring
  | Internal_error

module Source_printer : sig
  type t
  val make : with_padding:bool -> with_comments:bool -> unit -> t
  val add_terminal : t -> g terminal index -> unit

  type source = string
  type locations

  val flush : t -> locations * source
  (*val flush_only_source : t -> source*)
  val flush_only_source_to_channel : t -> out_channel -> unit

  val classify_error_location : locations -> Ocamlformat.location -> error_kind * int
end = struct
  let padding = String.make 90 ' '

  type location = int * int

  type t = {
    buffer: Buffer.t;
    with_padding: bool;
    mutable comments: int;
    mutable token_locations: location list;
    mutable comment_locations: location list;
  }

  let make ~with_padding ~with_comments () = {
    comments = if with_comments then 0 else (-1);
    with_padding;
    buffer = Buffer.create 63;
    token_locations = [];
    comment_locations = [];
  }

  let startp kind t =
    match Buffer.length t.buffer, kind with
    | 0, _ ->
      if t.with_padding then
        Buffer.add_string t.buffer padding;
      Buffer.length t.buffer
    | n, `Regular ->
      Buffer.add_char t.buffer ' ';
      (n + 1)
    | n, `Suffix -> n

  let endp t = Buffer.length t.buffer

  let add_comment kind t =
    match kind, t.comments with
    | `Regular, -1 | `Suffix, _  -> ()
    | `Regular, number ->
      t.comments <- number + 1;
      let startp = startp kind t in
      Printf.bprintf t.buffer "(* C%d *)" number;
      let endp = endp t in
      t.comment_locations <- (startp, endp) :: t.comment_locations

  let add_terminal t term =
    match terminal_text.:(term) with
    | "", _ -> ()
    | text, kind ->
      add_comment kind t;
      let startp = startp kind t in
      Buffer.add_string t.buffer text;
      let endp = endp t in
      t.token_locations <- (startp, endp) :: t.token_locations

  type source = string

  type locations = {
    tokens: (int * int) array;
    comments: (int * int) array;
  }

  let flush_source t =
    add_comment `Regular t;
    let source = Buffer.contents t.buffer in
    source

  let get_locations t =
    let prepare_locations l = Array.of_list (List.rev l) in
    let tokens = prepare_locations t.token_locations in
    let comments = prepare_locations t.comment_locations in
    {tokens; comments}

  let reset t =
    Buffer.clear t.buffer;
    if t.comments > -1 then
      t.comments <- 0;
    t.token_locations <- [];
    t.comment_locations <- []

  let flush t =
    let source = flush_source t in
    let locations = get_locations t in
    reset t;
    (locations, source)

  (*let flush_only_source t =
    let source = flush_source t in
    reset t;
    source*)

  let flush_only_source_to_channel t oc =
    add_comment `Regular t;
    Buffer.output_buffer oc t.buffer;
    reset t

  let classify_error_location l {Ocamlformat. line; start_col; end_col; _} =
    if line <> 1 then
      (Red_herring, Array.length l.tokens)
    else
      let find_start (startp, endp) = startp <= start_col && start_col < endp in
      let find_end (_, endp) = end_col = endp in
      match Array.find_index find_start l.tokens with
      | None ->
        let find_exact (startp, endp) = start_col = startp && endp = end_col in
        begin match Array.find_index find_exact l.comments with
          | Some i ->
            (* Exact comment match: comment (likely dropped) error *)
            (Comment, i)
          | None ->
            (* No match: red herring! *)
            (Red_herring, Array.length l.tokens)
        end
      | Some i ->
        let startp, endp = l.tokens.(i) in
        if startp = start_col && end_col = endp then
          (* Exact token match: raw syntax error *)
          (Syntax, i)
        else if end_col <= endp then
          (* Subsequence token match: lexer error *)
          (Lexer, i)
        else if startp = start_col && Array.exists find_end l.tokens then
          (* Range match: likely a syntactic invariant *)
          (Syntactic_invariant, i)
        else
          (* No match either *)
          (Red_herring, Array.length l.tokens)
end

module Sample_sentence_printer : sig
  type t

  val make : with_comment:bool -> position:int option -> t
  val add_terminal : t -> g terminal index -> unit
  val flush : t -> string list
end = struct
  type t = {
    with_comment: bool;
    position: int;
    buffer: Buffer.t;
    mutable count: int;
    mutable startp: int;
    mutable endp: int;
  }

  let make ~with_comment ~position = {
    with_comment;
    position = Option.value position ~default:(-1);
    buffer = Buffer.create 31;
    count = 0;
    startp = 0;
    endp = 0;
  }

  let startp kind t =
    match Buffer.length t.buffer, kind with
    | 0, _ -> 0
    | n, `Regular -> Buffer.add_char t.buffer ' '; (n + 1)
    | n, `Suffix -> n

  let add_terminal t term =
    let text, kind = terminal_text.:(term) in
    if t.count = -1 then
      invalid_arg "Sample_sentence_printer.add_terminal: buffer already flushed";
    if text <> "" then (
      let startp = startp kind t in
      if t.position = t.count then (
        let comment = t.with_comment && kind = `Regular in
        t.startp <- startp;
        Buffer.add_string t.buffer (if comment then "(* ... *)" else text);
        t.endp <- Buffer.length t.buffer;
        if comment then (
          Buffer.add_char t.buffer ' ';
          Buffer.add_string t.buffer text
        )
      ) else (
        Buffer.add_string t.buffer text
      );
      t.count <- t.count + 1
    )

  let flush t =
    let source = Buffer.contents t.buffer in
    if t.position > t.count then
      Printf.ksprintf invalid_arg "Sample_sentence_printer.flush: sentence is incomplete (error position:%d, with comment:%b, count:%d, text:%S)"
        t.position t.with_comment t.count source;
    if t.position = t.count then (
      t.startp <- Buffer.length t.buffer + 1;
      t.endp <- t.startp + 1
    );
    t.count <- -1;
    if t.endp > t.startp then
      [source; String.make t.startp ' ' ^ String.make (t.endp - t.startp) '^']
    else
      [source]
end

type derivation_kind = Ocamlformat.source_kind =
  | Impl
  | Intf

let derivation_kind der =
  let entrypoint = entrypoint_of_derivation der in
  match Symbol.name grammar entrypoint with
  | "implementation" -> Ocamlformat.Impl
  | "interface" -> Ocamlformat.Intf
  | name ->
    if report_invalid_entrypoint entrypoint then
      Syntax.warn Lexing.dummy_pos
        "ocamlformat-check: invalid entrypoint %s, \
         only implementation and interface are supported"
        name;
    Ocamlformat.Impl

let prepare_derivation_for_check printer der =
  Derivation.iter_terminals der ~f:(Source_printer.add_terminal printer);
  let locations, source = Source_printer.flush printer in
  (derivation_kind der, locations, source)

type error = {
  path: g item index list;
  source_kind: Ocamlformat.source_kind;
  derivation: (g, Reach.r, g item index list *
                           Reach.Cell.n index *
                           g item index list) Derivation.t;
  kind: error_kind;
  position: int option;
}

let report_error_samples oc ~with_comment errors =
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
  iter_by errors ~compare:compare_path begin fun i e es ->
    if i > !opt_max_errors_report then (
      Printf.fprintf oc "- ...\n";
      raise Exit_iter
    );
    Printf.fprintf oc "- Derivation (%d occurrence%s):\n"
      (1 + List.length es)
      (plural (e :: es));
    Printf.fprintf oc "  ```\n";
    List.iteri begin fun i x ->
      Printf.fprintf oc "  %s%s\n"
        (String.make (2 * i) ' ')
        (Item.to_string grammar x);
    end (List.rev e.path);
    Printf.fprintf oc "  ```\n";
    let sample = List.fold_left
        (fun e e' ->
           if Derivation.length e'.derivation <
              Derivation.length e.derivation
           then e'
           else e
        ) e es
    in
    Printf.fprintf oc "  Sample sentence (%s):\n"
      (match sample.source_kind with
       | Ocamlformat.Impl -> "implementation"
       | Intf -> "interface");
    Printf.fprintf oc "  ```ocaml\n";
    let position = sample.position in
    let printer = Sample_sentence_printer.make ~with_comment ~position in
    Derivation.iter_terminals sample.derivation
      ~f:(Sample_sentence_printer.add_terminal printer);
    List.iter (Printf.fprintf oc "  %s\n") (Sample_sentence_printer.flush printer);
    Printf.fprintf oc "  ```\n";
  end;
  Printf.fprintf oc "\n"

let report_located_errors ?(filter=fun _ -> true) oc derivations outcome =
  (* Filter and group common errors by message *)
  let by_message = Hashtbl.create 7 in
  Array.iteri begin fun i (source_kind, errors) ->
    if filter i then
      List.iter begin fun (kind, pos, error) ->
        match kind with
        | Internal_error | Red_herring -> ()
        | _ ->
          let expansion, reduction =
            match derivations.(i).Derivation.desc with
            | Expand {expansion; reduction} -> (expansion, reduction)
            | _ -> assert false
          in
          let derivation = Derivation.items_of_expansion grammar ~expansion ~reduction in
          let path, _cell, _path = Derivation.get_meta derivation pos in
          let err = {source_kind; derivation; kind; path; position = Some pos} in
          match Hashtbl.find_opt by_message error.Ocamlformat.message with
          | None -> Hashtbl.add by_message error.message (ref [err])
          | Some r -> push r err
      end errors
  end outcome;
  (* Order by number of occurrences *)
  let by_message =
    Array.of_seq (Seq.map (fun (k, v) -> (k, List.length !v, !v))
                    (Hashtbl.to_seq by_message))
  in
  Array.sort (fun (_,r1,_) (_,r2,_) -> Int.compare r2 r1) by_message;
  (* Classify by item and error kind *)
  let heap = Occurrence_heap.make (Item.cardinal grammar) in
  let all_syntax_errors = ref [] in
  let all_comment_errors = ref [] in
  let all_lexer_errors = ref [] in
  let all_invariant_errors = ref [] in
  let push_non_empty l r v =
    if not (List.is_empty l) then
      push r v
  in
  Array.iter begin fun (message, _, errors) ->
    (* Errors by most frequent items *)
    List.iter
      (fun e -> Occurrence_heap.add heap (IndexSet.of_list e.path) e)
      errors;
    let syntax_errors = ref [] in
    let comment_errors = ref [] in
    let lexer_errors = ref [] in
    let invariant_errors = ref [] in
    Seq.iter begin fun (item, errors) ->
      let kind (k : error_kind) e = e.kind = k in
      let lex', errors = List.partition (kind Lexer) errors in
      push_non_empty lex' lexer_errors (item, lex');
      let com', errors = List.partition (kind Comment) errors in
      push_non_empty com' comment_errors (item, com');
      push_non_empty errors
        (if List.exists (kind Syntactic_invariant) errors
         then invariant_errors
         else syntax_errors)
        (item, errors);
    end (Occurrence_heap.pop_seq heap);
    let push_result r v = push_non_empty !v r (message, List.rev !v) in
    push_result all_syntax_errors syntax_errors;
    push_result all_comment_errors comment_errors;
    push_result all_lexer_errors lexer_errors;
    push_result all_invariant_errors invariant_errors;
  end by_message;
  (* Report each major kind *)
  let report_kind ~with_comment title r ~header =
    match List.rev !r with
    | [] -> ()
    | group ->
      Printf.fprintf oc "# %s\n\n" title;
      header ();
      Printf.fprintf oc "\n\n";
      List.iter begin fun (message, errors) ->
        Printf.fprintf oc "## %s\n" message;
        List.iter begin fun (item, errors) ->
          Printf.fprintf oc "\n### Item `%s` (in %d error%s)\n\n"
            (Item.to_string grammar item)
            (List.length errors) (plural errors);
          report_error_samples oc ~with_comment errors;
        end errors
      end group;
      Printf.fprintf oc "\n"
  in
  report_kind ~with_comment:false "Parser errors"    all_syntax_errors
    ~header:begin fun () ->
      Printf.fprintf oc
        "A parser error is reported when OCamlformat rejects an input \
         on a specific token.\n\
         The error location is the token that caused the failure; \n\
         it is usually the exact point where the parser could not continue."
    end;
  report_kind ~with_comment:false "Lexer errors"     all_lexer_errors
    ~header:begin fun () ->
      Printf.fprintf oc
        "A lexer error is reported when OCamlformat rejected an input on a \
         location that does not form a complete token for the fuzzer.\n\
         This usually indicates a mismatch between the lexical specification \
         used by the fuzzer and the lexer implementation in OCamlformat.\n\
         The token at that spot is likely not properly recognized."
    end;
  report_kind ~with_comment:true  "Comment errors"   all_comment_errors
    ~header:begin fun () ->
      Printf.fprintf oc
        "These are errors OCamlformat reports while processing a comment.\n\
         They usually mean that the comment was not preserved by the formatting \
         process (e.g., it was dropped or moved)."
    end;
  report_kind ~with_comment:false "Invariant errors" all_invariant_errors
    ~header:begin fun () ->
      Printf.fprintf oc
        "Invariant errors are grammatical violations that span more than one \
         token and are detected by semantic actions after parsing.\n\
         They are not produced by Menhir itself but by checks that enforce \
         specific language invariants.\n\
         The reported location is typically the first token of the offending \
         construct.\n\
         Because the fuzzer does not understand these finer invariants, \
         such errors may appear as false positives."
    end

let report_non_located_errors ?(filter=fun _ -> true) oc derivations outcome kind ~header =
  (* Filter and group by error message *)
  let by_message = Hashtbl.create 7 in
  let highly_safe_cells = Boolvector.make Reach.Cell.n false in
  let is_unsafe_cell cell = not (Boolvector.test highly_safe_cells cell) in
  (*let potentially_safe_cells = Boolvector.make Reach.Cell.n false in
  let is_potentially_unsafe cell =
    not (Boolvector.test potentially_safe_cells cell)
    in*)
  (*let is_highly_unsafe cell =
    not (Boolvector.test highly_safe_cells cell) &&
    is_potentially_unsafe cell
    in*)
  let mark_safe table der =
    let rec loop der =
      Boolvector.set table (Derivation.meta der);
      Derivation.iter_sub loop der
    in
    loop der
  in
  let outcome' =
    outcome
    |> Array.to_seqi
    |> Seq.filter_map begin fun (i, (source_kind, errors)) ->
      match errors with
      | [] ->

        (* No error, mark all cells in this derivation as highly safe *)
        mark_safe highly_safe_cells derivations.(i);
        None

      | _ when not (filter i) ->
        None

      | errors ->

        let errors = List.filter (fun (k, _, _) -> k = kind) errors in

        (*if List.is_empty errors then (
          (* There were errors, but none of the kind we were looking for.
             Mark all cells in this derivation as potentially safe *)
          mark_safe potentially_safe_cells derivations.(i);
          None
          ) else*)
          Some (i, source_kind, errors)
    end
    |> Array.of_seq
  in
  (* Now that we know the remaining errors and the safe cells,
     classify the potential locations of the errors *)
  Array.iter begin fun (i, source_kind, errors) ->
    List.iter begin fun (_, _, {Ocamlformat. message; _}) ->
      let expansion, reduction =
        match derivations.(i).Derivation.desc with
        | Expand {expansion; reduction} -> (expansion, reduction)
        | _ -> assert false
      in
      let unsafe_items = ref IndexSet.empty in
      let add_item item = unsafe_items := IndexSet.add item !unsafe_items in
      let collect_cells pred der =
        let rec loop der =
          let path, cell, _path = Derivation.meta der in
          if pred cell then
            List.iter add_item path;
          Derivation.iter_sub loop der
        in
        loop der
      in
      let derivation = Derivation.items_of_expansion grammar ~expansion ~reduction in
      (* Try to find the worst offenders *)
      collect_cells is_unsafe_cell derivation;
      (*if IndexSet.is_empty !unsafe_items then
        (* Nothing found. Try to find potential offenders *)
        collect_cells is_potentially_unsafe derivation;*)
      if IndexSet.is_empty !unsafe_items then
        (* Still nothing. Try anything *)
        collect_cells (fun _ -> true) derivation;
      assert (not (IndexSet.is_empty !unsafe_items));
      let error = (!unsafe_items, {source_kind; derivation; kind; path = []; position = None}) in
      match Hashtbl.find_opt by_message message with
      | None -> Hashtbl.add by_message message (ref [error])
      | Some r -> push r error
    end errors
  end outcome';
  (* Order by number of occurrences *)
  let by_message =
    Array.of_seq (Seq.map (fun (k, v) -> (k, List.length !v, !v))
                    (Hashtbl.to_seq by_message))
  in
  Array.sort (fun (_,r1,_) (_,r2,_) -> Int.compare r2 r1) by_message;
  (* Classify by item *)
  let annotate_with_item item error =
    let path = ref [] in
    let pos = ref 0 in
    let find_pos pred der =
      let rec loop der =
        let pos' = !pos in
        Derivation.iter_sub loop der;
        let path', cell, _ = Derivation.meta der in
        if pred cell && List.mem item path' then (
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
        find_pos is_unsafe_cell error.derivation;
        (*find_pos is_potentially_unsafe error.derivation;*)
        find_pos (fun _ -> true) error.derivation;
      with Exit -> ()
    end;
    {error with path = !path}
  in
  let heap = Occurrence_heap.make (Item.cardinal grammar) in
  Array.iteri begin fun i (message, _, errors) ->
    if i = 0 then header oc;
    Printf.fprintf oc "## %s (%d error%s)\n" message (List.length errors) (plural errors);
    (* Errors by most frequent items *)
    List.iter
      (fun (items, error) -> Occurrence_heap.add heap items error)
      errors;
    Seq.iter (fun (item, errors) ->
        Printf.fprintf oc "\n### Item `%s` (in %d error%s)\n\n"
          (Item.to_string grammar item)
          (List.length errors) (plural errors);
        report_error_samples oc ~with_comment:false
          (List.map (annotate_with_item item) errors);
      ) (Occurrence_heap.pop_seq heap)
  end by_message;
  Printf.fprintf oc "\n"

(* default mode: just output a list of sentences *)

let print_mode () =
  let printer =
    Source_printer.make ~with_padding:false ~with_comments:!opt_comments ()
  in
  Seq.iter begin fun der ->
    if !opt_print_entrypoint then (
      let entrypoint =
        match der.Derivation.desc with
        | Expand {expansion=_; reduction} ->
          let prod = reduction.Reachability.production in
          Production.lhs grammar prod
        | _ ->
          prerr_endline "Cannot locate entrypoint in derivation:";
          Derivation_printer.output stderr (Derivation.print grammar der);
          prerr_newline ();
          assert false
      in
      output_string stdout (Nonterminal.to_string grammar entrypoint);
      output_string stdout ": ";
    );
    Derivation.iter_terminals ~f:(Source_printer.add_terminal printer) der;
    Source_printer.flush_only_source_to_channel printer stdout;
    output_char stdout '\n'
  end derivations

(* check mode: stress ocamlformat, classify outputs *)

let internal_error_header oc =
  Printf.fprintf oc "# Internal errors\n\n";
  Printf.fprintf oc
    "When OCamlformat fails with an internal error, the exact location of \
     the problem cannot be determined.\n\
     The location is guessed by examining the syntactic constructions \
     that appear most frequently in the failing code.\n\n"

let red_herring_header oc =
  Printf.fprintf oc "# Red herrings\n\n";
  Printf.fprintf oc
    "Red herrings are errors for which OCamlformat reports an invalid location.\n\
     This can happen, for example, if the formatter succeeds on the first pass but \
     fails on a subsequent one, causing the error to refer to a location in an \
     intermediate file that is not visible to end users.\n\
     Note that, as with internal errors, the exact location of the problem \
     cannot be determined.\n\
     The location is guessed by inspecting the syntactic constructions that appear \
     most frequently in the failing code.\n\n"

type stats = {
  valid: int;
  syntax_errors: int;
  with_comments_dropped: int;
  comments_dropped: int;
  internal_errors: int;
}

let report_errors ?filter oc derivations outcome =
  report_located_errors ?filter oc derivations outcome;
  report_non_located_errors ?filter oc derivations outcome Internal_error
    ~header:internal_error_header;
  report_non_located_errors ?filter oc derivations outcome Red_herring
    ~header:red_herring_header

let track_regressions path_from path_to path_report derivations outcome stats =
  let result = ref true in
  let header = "OCAMLGRAMMARFUZZER0" in
  let hash = Digest.to_hex (Digest.string cmly_content) in
  let trailer = "END" in
  if path_from <> "" then (
    if Sys.file_exists path_from then (
      let ic = open_in_bin path_from in
      begin try
          if input_line ic <> header then
            failwith "invalid file format";
          let hash' = input_line ic in
          if hash <> hash' then
            failwithf "different grammar (hash: %s, expected: %s)"
              hash' hash;
          let count = int_of_string (input_line ic) in
          if count <> Array.length outcome then
            failwithf "different number of sentences (got: %d, expected: %d)"
              count (Array.length outcome);
          let line = input_line ic in
          Scanf.sscanf line
            "valid:%d syntax_errors:%d with_comments_dropped:%d \
             comments_dropped:%d internal_errors:%d"
            (fun valid syntax_errors with_comments_dropped
                 comments_dropped internal_errors ->
              let print name high_is_better before after =
                match after - before with
                | 0 -> ()
                | delta ->
                   let improvement = if high_is_better then delta > 0 else delta < 0 in
                   Printf.printf "Change in %s: %+d (%s)\n" name delta
                     (if improvement then "improvement" else "REGRESSION")
              in
              print "valid" true
                valid stats.valid;
              print "syntax errors" false
                syntax_errors stats.syntax_errors;
              print "with comments dropped" false
                with_comments_dropped stats.with_comments_dropped;
              print "total comments dropped" false
                comments_dropped stats.comments_dropped;
              print "internal errors" false
                internal_errors stats.internal_errors;
            );
          let current_line = ref 0 in
          let reported = ref 0 in
          let printer = Source_printer.make ~with_padding:false ~with_comments:!opt_comments () in
          let check_successes limit =
            while !current_line < limit do
              let index = !current_line in
              incr current_line;
              let _, errors = outcome.(index) in
              if not (List.is_empty errors) then (
                result := false;
                if !reported < !opt_max_errors_report then (
                  print_string "Regression: ";
                  Derivation.iter_terminals derivations.(index)
                    ~f:(Source_printer.add_terminal printer);
                  Source_printer.flush_only_source_to_channel printer stdout;
                  print_newline ();
                ) else if !reported = !opt_max_errors_report then
                  print_endline "regression: ...";
                incr reported
              )
            done
          in
          let previously_ok = Array.make (Array.length derivations) true in
          let failed line =
            previously_ok.(line) <- false;
            check_successes line;
            incr current_line;
            assert (!current_line = line + 1);
          in
          let rec loop () =
            match input_line ic with
            | line when line = trailer -> ()
            | line ->
               failed (int_of_string line);
               loop ()
          in
          loop ();
          check_successes (Array.length outcome);
          if path_report <> "" then (
            let oc = open_out_bin path_report in
            report_errors oc ~filter:(Array.get previously_ok) derivations outcome;
            close_out_noerr oc;
          );
        with exn ->
          result := false;
          let msg = match exn with
            | Failure str -> str
            | End_of_file -> "unexpected end of file"
            | exn -> "unhandled exception: " ^ Printexc.to_string exn
          in
          Printf.eprintf "Regressions: %s\n" msg;
      end;
      close_in_noerr ic
    ) else
      result := false;
  );
  if path_to <> "" then (
    let oc = open_out_bin path_to in
    let p fmt = Printf.fprintf oc fmt in
    p "%s\n%s\n%d\n" header hash (Array.length outcome);
    p "valid:%d syntax_errors:%d with_comments_dropped:%d \
       comments_dropped:%d internal_errors:%d\n"
      stats.valid stats.syntax_errors stats.with_comments_dropped
      stats.comments_dropped stats.internal_errors;
    Array.iteri begin fun i (_, errors) ->
      if not (List.is_empty errors) then
        p "%d\n" i
      end outcome;
    p "%s\n" trailer;
    close_out_noerr oc;
  );
  !result

let check_mode () =
  let outputs = ref [] in
  let get_output = function
    | "" -> invalid_arg "get_output: empty name"
    | "-" -> stdout
    | name ->
      match List.assoc_opt name !outputs with
      | Some oc -> oc
      | None ->
        let oc = open_out_bin name in
        push outputs (name, oc);
        oc
  in
  let close_outputs () =
    List.iter (fun (_, oc) -> close_out oc) !outputs;
    outputs := []
  in
  let derivations = Array.of_seq derivations in
  let sources =
    let printer = Source_printer.make ~with_padding:true ~with_comments:!opt_comments () in
    Array.map (prepare_derivation_for_check printer) derivations
  in
  let outcome =
    Array.to_seq sources
    |> Seq.map (fun (k,_,s)  -> (k, s))
    |> Ocamlformat.check
      ~ocamlformat_command:!opt_ocamlformat
      ~jobs:(Int.max 0 !opt_jobs)
      ~batch_size:(Int.max 1 !opt_batch_size)
      ?debug_line:(if !opt_debug_log_output then
                     Some prerr_endline
                   else None)
    |> Seq.mapi begin fun i errors ->
      let source_kind, locations, _ = sources.(i) in
      let errors =
        List.map begin fun error ->
          let kind, position = match error.Ocamlformat.location with
            | None -> (Internal_error, -1)
            | Some loc ->
              Source_printer.classify_error_location locations loc
          in
          (kind, position, error)
        end errors
      in
      (source_kind, errors)
    end
    |> Array.of_seq
  in
  (* Report all errors *)
  report_errors (get_output !opt_save_report) derivations outcome;
  (* Summarize results *)
  let valid = ref 0 in
  let syntax_errors = ref 0 in
  let with_comments_dropped = ref 0 in
  let comments_dropped = ref 0 in
  let internal_errors = ref 0 in
  Array.iter begin fun (_, errors) ->
    match errors with
    | [] -> incr valid;
    | errors ->
      let had_comments_dropped = ref false in
      List.iter begin fun (kind, _, _) ->
        match kind with
        | Syntax | Lexer | Syntactic_invariant -> incr syntax_errors;
        | Comment ->
          if not !had_comments_dropped then (
            incr with_comments_dropped;
            had_comments_dropped := true;
          );
          incr comments_dropped;
        | Red_herring | Internal_error ->
          incr internal_errors
      end errors
  end outcome;
  let stats = {
    valid                 = !valid;
    syntax_errors         = !syntax_errors;
    with_comments_dropped = !with_comments_dropped;
    comments_dropped      = !comments_dropped;
    internal_errors       = !internal_errors;
  } in
  let count = Array.length outcome in
  let percent x = 100.0 *. float x /. float count in
  Printf.eprintf "Tested %d sentences:\n\
                  - %d successfully formated (%.02f%%)\n\
                  - %d failed with syntax errors (%.02f%%)\n\
                  - %d had comments dropped (%.02f%%) (%d comments were dropped in total)\n\
                  - %d caused internal errors (%.02f%%)\n%!"
    count
    stats.valid            (percent stats.valid)
    stats.syntax_errors    (percent stats.syntax_errors)
    stats.with_comments_dropped (percent stats.with_comments_dropped) stats.comments_dropped
    stats.internal_errors  (percent stats.internal_errors);
  (* Save sentences by error class *)
  let output_sentences path pred =
    if path <> "" then
      let oc = get_output path in
      Array.iteri begin fun i (_, errors) ->
        if pred errors then
          let kind, _, source = sources.(i) in
          let prefix = match kind with
            | Intf -> "interface: "
            | Impl -> "implementation: "
          in
          output_string oc prefix;
          output_string oc (String.trim source);
          output_char oc '\n'
        end outcome
  in
  let has_kind kind errors =
    List.exists (fun (kind', _, _) -> kind = kind') errors
  in
  output_sentences !opt_save_successful
    (List.is_empty);
  output_sentences !opt_save_lexer_errors
    (has_kind Lexer);
  output_sentences !opt_save_parser_errors
    (has_kind Syntax);
  output_sentences !opt_save_invariant_errors
    (has_kind Syntactic_invariant);
  output_sentences !opt_save_internal_errors
    (fun errs -> has_kind Internal_error errs ||
                 has_kind Red_herring errs);
  output_sentences !opt_save_comment_errors (has_kind Comment);
  flush stdout;
  (* Track regressions *)
  let result =
    track_regressions
      !opt_track_regressions_from !opt_track_regressions_to
      !opt_regressions_report_to
      derivations outcome stats;
  in
  close_outputs ();
  if result || !opt_regressions_non_fatal then
    exit 0
  else
    exit 1


let () =
  if !opt_ocamlformat_check then
    check_mode ()
  else
    print_mode ()
