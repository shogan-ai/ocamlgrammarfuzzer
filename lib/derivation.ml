open Fix.Indexing
open Info

type ('g, 'r, 'a) t = {
  cell: 'a;
  desc: ('g, 'r, 'a) desc;
}
and ('g, 'r, 'a) desc =
  | Null
  | Shift of 'g terminal index
  | Node of {
      left: ('g, 'r, 'a) t;
      right: ('g, 'r, 'a) t;
      length: int;
    }
  | Expand of {
      expansion: ('g, 'r, 'a) t;
      reduction: 'g Reachability.reduction;
    }

let cell t = t.cell

let rec length t = match t.desc with
  | Null -> 0
  | Shift _ -> 1
  | Node {length; _} -> length
  | Expand {expansion; _} -> length expansion

let iter_sub f t = match t.desc with
  | Null | Shift _ -> ()
  | Node {left; right; _} -> f left; f right
  | Expand {expansion; _} -> f expansion

let null cell = {cell; desc = Null}

let shift cell terminal =
  {cell; desc = Shift terminal}

let node cell left right =
  let length = length left + length right in
  {cell; desc = Node {left; right; length}}

let expand cell expansion reduction =
  {cell; desc = Expand {expansion; reduction}}

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

let rec get_cell t i =
  match t.desc with
  | Expand {expansion; _} -> get_cell expansion i
  | Null | Shift _ ->
    if i > 0 then raise Not_found;
    t.cell
  | Node {left; right; _} ->
    let len = length left in
    if i < len then
      get_cell left i
    else
      get_cell right (i - len)

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

type ('g, 'r, 'a) path =
  | Left_of of {
      right: 'a;
      cell: 'r Reachability.cell index;
    }
  | Right_of of {
      left: 'a;
      cell: 'r Reachability.cell index;
    }
  | In_expansion of {
      reduction: 'g Reachability.reduction;
      cell: 'r Reachability.cell index;
    }

let map_path f = function
  | Left_of {right; cell} ->
    let right = f right in
    Left_of {right; cell}
  | Right_of {left; cell} ->
    let left = f left in
    Right_of {left; cell}
  | In_expansion _ as x -> x

let unroll_path der = function
  | Left_of {right; cell} ->
    node cell der right
  | Right_of {left; cell} ->
    node cell left der
  | In_expansion {reduction; cell} ->
    expand cell der reduction

let items_of_expansion g ~expansion ~reduction =
  let rec aux parents prod pos t =
    let item = Item.make g prod pos in
    let parents = if pos = 0 then item :: parents else [item] in
    let cell = (t.cell, parents) in
    match t.desc with
    | Null | Shift _ as desc ->
      pos + 1, {cell; desc}
    | Expand e ->
      let _, expansion = aux parents e.reduction.production 0 e.expansion in
      pos + 1, {cell; desc = Expand {e with expansion}}
    | Node n ->
      let cell = (t.cell, []) in
      let pos, left = aux parents prod pos n.left in
      let pos, right = aux [] prod pos n.right in
      pos, {cell; desc = Node {n with left; right}}
  in
  let {Reachability.production; _} = reduction in
  let _ , der = aux [] production 0 expansion in
  der
