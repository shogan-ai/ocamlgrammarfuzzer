[@@@warning "-33"]
open Fix.Indexing
open Utils
open Misc

module Grammar = MenhirSdk.Cmly_read.FromString(Ocaml_grammar)
module Info = Grammarfuzzer.Info.Make(Grammar)
module Reachability = Grammarfuzzer.Reachability.Make(Info)()
module Lrc_raw = Grammarfuzzer.Lrc.Make(Info)(Reachability)()

let () = Random.self_init ()

let sample set =
  let index = ref (Random.int (IndexSet.cardinal set)) in
  IndexSet.find_map (fun elt ->
      if !index = 0
      then Some elt
      else (decr index; None)
    ) set
  |> Option.get

open Lrc_raw

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
  Index.iter TrC.goto (fun gt ->
      let reds = RedC.from_target gt in
      let cost = TrC.cost (TrC.of_goto gt) in
      let cost' =
        IndexSet.fold
          (fun red cost' -> Int.min cost' (RedC.cost red))
          reds Int.max_int
      in
      if cost <> cost' then (
        Printf.eprintf "transition %d:%s->%s should have %d but reductions permit reaching %d:\n"
          (gt :> int)
          (LrC.to_string (TrC.source (TrC.of_goto gt)))
          (LrC.to_string (TrC.target (TrC.of_goto gt)))
          cost cost';
        IndexSet.iter (fun red ->
            Printf.eprintf "- cost %d: %s\n"
              (RedC.cost red)
              (string_of_prod (RedC.production red))
          ) reds;
      )
    )
