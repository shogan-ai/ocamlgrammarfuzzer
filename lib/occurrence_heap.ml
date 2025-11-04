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
