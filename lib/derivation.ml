(**************************************************************************)
(*                                                                        *)
(*        OCamlgrammarfuzzer © 2025 by Frédéric Bour, Shogan.ai          *)
(*                                                                        *)
(*                     SPDX-License-Identifier: MIT                       *)
(*                   See the LICENSE file for details.                    *)
(*                                                                        *)
(**************************************************************************)

open Fix.Indexing
open Info

type ('g, 'r, 'm) t = {
  meta: 'm;
  desc: ('g, 'r, 'm) desc;
}
and ('g, 'r, 'm) desc =
  | Null
  | Shift of 'g terminal index
  | Node of {
      left: ('g, 'r, 'm) t;
      right: ('g, 'r, 'm) t;
      length: int;
    }
  | Expand of {
      expansion: ('g, 'r, 'm) t;
      reduction: 'g Reachability.reduction;
    }

let meta t = t.meta

let rec length t = match t.desc with
  | Null -> 0
  | Shift _ -> 1
  | Node {length; _} -> length
  | Expand {expansion; _} -> length expansion

let iter_sub f t = match t.desc with
  | Null | Shift _ -> ()
  | Node {left; right; _} -> f left; f right
  | Expand {expansion; _} -> f expansion

let null meta = {meta; desc = Null}

let shift meta terminal =
  {meta; desc = Shift terminal}

let node meta left right =
  let length = length left + length right in
  {meta; desc = Node {left; right; length}}

let expand meta expansion reduction =
  {meta; desc = Expand {expansion; reduction}}

let rec get_terminal t i =
  match t.desc with
  | Expand {expansion; _} -> get_terminal expansion i
  | Null -> raise Not_found
  | Shift terminal ->
    if i > 0 then raise Not_found;
    terminal
  | Node {left; right; _} ->
    let len = length left in
    if i < len then
      get_terminal left i
    else
      get_terminal right (i - len)

let rec get_meta t i =
  match t.desc with
  | Expand {expansion; _} -> get_meta expansion i
  | Null | Shift _ ->
    if i > 0 then raise Not_found;
    t.meta
  | Node {left; right; _} ->
    let len = length left in
    if i < len then
      get_meta left i
    else
      get_meta right (i - len)

let terminals der =
  let rec loop acc t =
    match t.desc with
    | Null -> acc
    | Shift terminal -> terminal :: acc
    | Node  {left; right; _} -> loop (loop acc right) left
    | Expand {expansion; _} -> loop acc expansion
  in
  loop [] der

let rec iter_terminals ~f t =
  match t.desc with
  | Null -> ()
  | Shift terminal -> f terminal
  | Node {left; right; _} ->
    iter_terminals ~f left;
    iter_terminals ~f right
  | Expand {expansion; _} ->
    iter_terminals ~f expansion

type ('g, 'm, 'a) path =
  | Left_of of {
      right: 'a;
      meta: 'm;
    }
  | Right_of of {
      left: 'a;
      meta: 'm;
    }
  | In_expansion of {
      reduction: 'g Reachability.reduction;
      meta: 'm;
    }

let map_path f = function
  | Left_of {right; meta} ->
    let right = f right in
    Left_of {right; meta}
  | Right_of {left; meta} ->
    let left = f left in
    Right_of {left; meta}
  | In_expansion _ as x -> x

let unroll_path der = function
  | Left_of {right; meta} ->
    node meta der right
  | Right_of {left; meta} ->
    node meta left der
  | In_expansion {reduction; meta} ->
    expand meta der reduction

let items_of_expansion g ~expansion ~reduction =
  let rec aux prefix prod pos suffix t =
    match t.desc with
    | Null | Shift _ as desc ->
      let prefix = Item.make g prod pos :: prefix in
      let pos = pos + 1 in
      let suffix = Item.make g prod pos :: suffix in
      let meta = (prefix, t.meta, suffix) in
      (pos, {meta; desc})
    | Expand e ->
      let prefix = Item.make g prod pos :: prefix in
      let pos = pos + 1 in
      let suffix = Item.make g prod pos :: suffix in
      let meta = (prefix, t.meta, suffix) in
      let _, expansion = aux prefix e.reduction.production 0 suffix e.expansion in
      (pos, {meta; desc = Expand {e with expansion}})
    | Node n ->
      let meta = ([], t.meta, []) in
      let pos, left = aux prefix prod pos [] n.left in
      let pos, right = aux [] prod pos suffix n.right in
      (pos, {meta; desc = Node {n with left; right}})
  in
  let {Reachability.production; _} = reduction in
  let _ , der = aux [] production 0 [] expansion in
  der

open Utils

let tilde = Derivation_printer.node "~" []
let lbra = Derivation_printer.node "[" []
let rbra = Derivation_printer.node "]" []

let rec print acc g der =
  match der.desc with
  | Null -> tilde :: acc
  | Shift t -> Derivation_printer.node (Terminal.to_string g t) [] :: acc
  | Node {left; right; _} ->
    lbra :: print (print (rbra :: acc) g right) g left
  | Expand {expansion; reduction} ->
    let nodes = print [] g expansion in
    let prod = reduction.Reachability.production in
    let nt = Production.lhs g prod in
    Derivation_printer.node (Nonterminal.to_string g nt) nodes :: acc

let print g der = print [] g der
