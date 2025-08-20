open Fix.Indexing
open Info

type ('g, 'r) t =
  | Null of {
      cell: 'r Reachability.cell index;
    }
  | Shift of {
      cell: 'r Reachability.cell index;
      terminal: 'g terminal index;
    }
  | Node of {
      cell: 'r Reachability.cell index;
      left: ('g, 'r) t;
      right: ('g, 'r) t;
      length: int;
    }
  | Expand of {
      cell: 'r Reachability.cell index;
      expansion: ('g, 'r) t;
      reduction: 'g Reachability.reduction;
    }

let cell ( Null {cell} | Shift {cell; _}
         | Node {cell; _} | Expand {cell; _} ) =
  cell

let rec length = function
  | Null _ -> 0
  | Shift _ -> 1
  | Node {length; _} -> length
  | Expand {expansion; _} -> length expansion

let iter_sub f = function
  | Null _ | Shift _ -> ()
  | Node {left; right; _} -> f left; f right
  | Expand {expansion; _} -> f expansion

let null cell = Null {cell}

let shift cell terminal =
  Shift {cell; terminal}

let node cell left right =
  let length = length left + length right in
  Node {cell; left; right; length}

let expand cell expansion reduction =
  Expand {cell; expansion; reduction}

let terminals der =
  let rec loop acc = function
    | Null _ -> acc
    | Shift  {terminal; _} -> terminal :: acc
    | Node   {left; right; _} -> loop (loop acc right) left
    | Expand {expansion; _} -> loop acc expansion
  in
  loop [] der

let rec iter_terminals ~f = function
  | Null _ -> ()
  | Shift  {terminal; _} -> f terminal
  | Node   {left; right; _} ->
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
    Expand {expansion = der; reduction; cell}
