[@@@warning "-33"]
open Fix.Indexing
open Utils
open Misc

module Grammar = MenhirSdk.Cmly_read.FromString(Ocaml_grammar)
module Info = Grammarfuzzer.Info.Make(Grammar)
module Reachability = Grammarfuzzer.Reachability.Make(Info)()

open Info

let () = Random.self_init ()

let sample_list l =
  List.nth l (Random.int (List.length l))

let rec fuzz size0 cell acc =
  let current_cost = Reachability.Cells.cost cell in
  let size = Int.max size0 current_cost in
  let node, i_pre, i_post = Reachability.Cells.decode cell in
  match Reachability.Tree.split node with
  | R (l, r) ->
    let coercion =
      Reachability.Coercion.infix (Reachability.Tree.post_classes l)
        (Reachability.Tree.pre_classes r)
    in
    let l_index = Reachability.Cells.encode l in
    let r_index = Reachability.Cells.encode r in
    let candidates = ref [] in
    Array.iteri begin fun i_post_l all_pre_r ->
      let cl = l_index i_pre i_post_l in
      let l_cost = Reachability.Cells.cost cl in
      if l_cost < max_int then
        Array.iter begin fun i_pre_r ->
          let cr = r_index i_pre_r i_post in
          let r_cost = Reachability.Cells.cost cr in
          if r_cost < max_int && l_cost + r_cost <= size then begin
            push candidates (cl, cr)
          end
        end all_pre_r
    end coercion.Reachability.Coercion.forward;
    let (cl, cr) = sample_list !candidates in
    let sl = Reachability.Cells.cost cl in
    let sr = Reachability.Cells.cost cr in
    let size = size - sl - sr in
    let mid =
      if Reachability.Finite.get cl then
        0
      else if Reachability.Finite.get cr then
        size
      else
        (Random.int (size + 1) + Random.int (size + 1)) / 2
    in
    fuzz (sl + mid) cl @@ fuzz (sr + (size - mid)) cr @@ acc
  | L tr ->
    match Transition.split tr with
    | R shift ->
      (* It is a shift transition, just shift the symbol *)
      Transition.shift_symbol shift :: acc
    | L goto ->
      (* It is a goto transition *)
      let nullable, non_nullable = Reachability.Tree.goto_equations goto in
      let c_pre = (Reachability.Tree.pre_classes node).(i_pre) in
      let c_post = (Reachability.Tree.post_classes node).(i_post) in
      let nullable =
        (* Is a nullable reduction is possible, don't do anything *)
        not (IndexSet.is_empty nullable) &&
        IndexSet.quick_subset c_post nullable &&
        not (IndexSet.disjoint c_pre c_post)
      in
      if size = 0 && nullable then
        acc
      else
        let candidates =
          List.filter_map begin fun (node', lookahead) ->
            if IndexSet.disjoint c_post lookahead then
              (* The post lookahead class does not permit reducing this
                 production *)
              None
            else
              let costs = Reachability.Cells.table.:(node') in
              match Reachability.Tree.pre_classes node' with
              | [|c_pre'|] when IndexSet.disjoint c_pre' c_pre ->
                (* The pre lookahead class does not allow to enter this
                   branch. *)
                None
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
                  Misc.array_findi pred_post 0 (Reachability.Tree.post_classes node')
                with
                | exception Not_found -> None
                | i_pre', i_post' ->
                  let offset = Reachability.Cells.offset node' i_pre' i_post' in
                  if costs.(offset) <= size
                  then Some (Reachability.Cells.encode_offset node' offset)
                  else None
          end non_nullable
        in
        let length = List.length candidates in
        if nullable then
          let index = Random.int (1 + length * (size + 1)) in
          if index = 0 then
            acc
          else
            fuzz size (List.nth candidates ((index - 1) / (size + 1))) acc
        else
          fuzz size (sample_list candidates) acc


let () =
  IndexSet.iter (fun gt ->
      let tr = Transition.of_goto gt in
      assert (Array.length (Reachability.Classes.pre_transition tr) = 1);
      assert (Array.length (Reachability.Classes.post_transition tr) = 1);
      let node = Reachability.Tree.leaf tr in
      let cell = Reachability.Cells.encode node 0 0 in
      let tokens = fuzz 100 cell [] in
      Printf.printf "%d tokens:" (List.length tokens);
      List.iter (fun t ->
          print_char ' ';
          print_string (Info.Terminal.to_string t)
        ) tokens;
      print_newline ();
      raise Exit
    ) Transition.accepting

(*let sample set =
  let index = ref (Random.int (IndexSet.cardinal set)) in
  IndexSet.find_map (fun elt ->
      if !index = 0
      then Some elt
      else (decr index; None)
    ) set
  |> Option.get

let reductions_for_size size gt =
  let reductions = RedC.from_target gt in
  let cost = TrC.cost (TrC.of_goto gt) in
  let size = Int.max size cost in
  IndexSet.filter (fun red -> RedC.cost red <= size) reductions

let rec fuzz size gt acc =
  let reduction = sample (reductions_for_size size gt) in
  let path = RedC.path reduction in
  let size, flexible =
    if size > 0 then
      List.fold_left
        (fun (size, flexible) tr -> (size - TrC.cost tr, flexible + 1))
        (size, 0) path
    else
      (size, 0)
  in
  Printf.eprintf "size: %d\n" size;
  if size <= 0 then
    List.fold_right (fun tr acc ->
        match TrC.split tr with
        | R sh -> TrC.shift_symbol sh :: acc
        | L gt -> fuzz 0 gt acc
      ) path acc
  else
    let splits =
      List.sort Int.compare
        (List.init flexible (fun _ -> Random.int (size + 1)))
    in
    let acc, _ =
      List.fold_right (fun tr (acc, splits) ->
          match TrC.split tr with
          | R sh -> (TrC.shift_symbol sh :: acc, splits)
          | L gt ->
            let size, splits =
              match splits with
              | [] -> assert false
              | [x] -> (size - x, [])
              | x :: (y :: _ as splits) -> (y - x, splits)
            in
            (fuzz (TrC.cost tr + size) gt acc, splits)
        ) path (acc, splits)
    in
    acc

let () =
  let tokens = fuzz 100 (IndexSet.choose TrC.accepting) [] in
  List.iter (fun t ->
      print_char ' ';
      print_string (Info.Terminal.to_string t)
    ) tokens

let string_of_prod prod =
  let open Info in
  Nonterminal.to_string (Production.lhs prod) ^ ": " ^
  match Production.rhs prod with
  | [||] -> "ϵ"
  | rhs -> string_concat_map " " Symbol.name (Array.to_list rhs)

let () =
  let ok = ref 0 in
  Index.iter TrC.goto (fun gt ->
      let reds = RedC.from_target gt in
      let cost = TrC.cost (TrC.of_goto gt) in
      let cost' =
        IndexSet.fold
          (fun red cost' -> Int.min cost' (RedC.cost red))
          reds Int.max_int
      in
      if cost <> cost' then (
        if cost' < max_int then
          Printf.eprintf "Transition %s->%s is expected to have cost %d but the reductions reaching it have cost %d:\n"
            (LrC.to_string (TrC.source (TrC.of_goto gt)))
            (LrC.to_string (TrC.target (TrC.of_goto gt)))
            cost cost'
        else
          Printf.eprintf "Transition %s->%s is expected to have cost %d but is unreachable\n"
            (LrC.to_string (TrC.source (TrC.of_goto gt)))
            (LrC.to_string (TrC.target (TrC.of_goto gt)))
            cost;
        IndexSet.iter (fun red ->
            Printf.eprintf "- cost %d: %s\n"
              (RedC.cost red)
              (string_of_prod (RedC.production red))
          ) reds;
        Printf.eprintf "looking ahead at %s\n"
          (string_of_indexset ~index:Info.Terminal.to_string (LrC.lookaheads (TrC.target (TrC.of_goto gt))));
      ) else incr ok
    );
  Printf.eprintf "%d transitions were correct\n" !ok*)
