[@@@warning "-33"]
open Fix.Indexing
open Utils
open Misc
open Grammarfuzzer

let opt_count = ref 1
let opt_length = ref 100
let opt_comments = ref false
let opt_seed = ref (-1)
let opt_jane = ref false
let opt_entrypoints = ref []
let opt_weights = ref []
let opt_avoid = ref []
let opt_focus = ref []

let spec_list = [
  (* ("-n"         , Arg.Set_int opt_count, "<int> Number of lines to generate"  ); *)
  ("--count"    , Arg.Set_int opt_count, "<int> Number of lines to generate"  );
  (* ("-c"         , Arg.Set opt_comments , " Generate fake comments in the lines"); *)
  ("--comments" , Arg.Set opt_comments , " Generate fake comments in the lines");
  (* ("-l"         , Arg.Set_int opt_length, "<int> Number of token per sentence"); *)
  ("--length"   , Arg.Set_int opt_length, "<int> Number of token per sentence");
  ("--seed"     , Arg.Set_int opt_seed, "<int> Random seed");
  ("--jane"     , Arg.Set opt_jane, " Use Jane Street dialect of OCaml");
  ("-v"         , Arg.Unit (fun () -> incr Stopwatch.verbosity), " Increase verbosity");
  ("--entrypoint", Arg.String (push opt_entrypoints), " Generate sentences from this entrypoint");
  ("--weight", Arg.String (push opt_weights), " Adjust the weights of grammatical constructions");
  ("--avoid", Arg.String (push opt_avoid), " Forbid grammatical constructions");
  ("--focus", Arg.String (push opt_focus), " Generate sentences stressing a grammatical construction");
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
      if !opt_jane
      then Ocaml_jane_grammar.content
      else Ocaml_grammar.content
  end)

include Info.Lift(Grammar)
open Info

(* Parse weight specifications *)

let weights = Vector.make (Production.cardinal grammar) 1.0

(* Compute reachability, considering only productions with strictly positive
   weights. *)
let reachability = Reachability.make grammar ~avoid:(fun prod -> weights.:(prod) <= 0.0)

module Reach = (val reachability)

(* Naive fuzzing *)

let sample_list l =
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

(* [fuzz size0 cell ~f] generates a sentence in the langauge recognized by
   [cell] that tries to have size [size0].
   The callback [f] is called for each terminal of the sentence, in order,
   and [fuzz] returns the number of terminals actually generated. *)
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
    let left_length = fuzz (sl + mid) cl ~f in
    (* Bias right side:
       - first set an expectation on the position of the split
         between left and right side,
       - generate left side targetting the split position
       - compensate on right side if left side missed expectation
    *)
    let right_length = fuzz (size - left_length) cr ~f in
    left_length + right_length
  | L tr ->
    match Transition.split grammar tr with
    | R shift ->
      (* It is a shift transition, just shift the symbol *)
      f (Transition.shift_symbol grammar shift);
      1
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
        0
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
              push candidates (Some cell, weights.:(reduction.production))
          );
        match sample_list !candidates with
        | None -> 0
        | Some cell -> fuzz size cell ~f

let () = Misc.stopwatch 0 "Start BFS"

let bfs = Vector.make Reach.Cell.n ([], [])

let () =
  IndexSet.iter (fun entrypoint ->
      begin match Lr1.is_entrypoint grammar entrypoint with
        | None -> assert false
        | Some production ->
          Printf.eprintf "Entrypoint: %s\n"
          (Nonterminal.to_string grammar (Production.lhs grammar production))
      end
    ) (Lr1.entrypoints grammar)

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
        iter_eqns i_pre i_post gt
          ~f:(fun reduction cell ->
              if weights.:(reduction.production) > 0.0 then
                visit path cell)
  in
  IndexSet.iter (fun tr ->
      let node = Reach.Tree.leaf (Transition.of_goto grammar tr) in
      visit ([],[]) (Reach.Cell.encode node ~pre:0 ~post:0)
    ) (Transition.accepting grammar);
  let counter = ref 0 in
  fixpoint ~counter ~propagate todo;
  Misc.stopwatch 0 "Stop BFS (depth: %d)" !counter

(* Check we know how to print each terminal *)

let terminals =
  let unknown = ref [] in
  let table =
    Vector.init (Terminal.cardinal grammar) @@
    fun t ->
    let name = Terminal.to_string grammar t in
    match Token_printer.print_token name with
    | txt -> txt
    | exception Not_found ->
      push unknown name; name
  in
  match !unknown with
  | [] -> table
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
    let _ : int =
      generate_sentence ~length:!opt_length
        (if !opt_comments
         then output_with_comments stdout
         else directly_output stdout)
    in
    output_char stdout '\n'
  done
