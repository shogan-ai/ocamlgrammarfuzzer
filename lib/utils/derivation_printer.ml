type 'a node = {
  label: 'a;
  width: int;
  pad: int;
  child_left: int;
  child_width: int;
  child_right: int;
  child: 'a node list;
}

type 'a row = {
  nodes: 'a node list;
  extra_left: int;
  extra_right: int;
}

let of_node node =
  let delta = node.child_width - node.width in
  let extra_left = Int.max 0 (delta / 2) + node.child_left in
  let extra_right = Int.max 0 (delta - delta / 2) + node.child_right in
  { nodes = [node]; extra_left; extra_right }

let width_of = function
  | [] -> 0
  | leaves -> List.fold_left (fun sum leaf -> sum + leaf.pad + leaf.width) 0 leaves

let join left leaves right =
  let interspace = 1 + List.length leaves + width_of leaves in
  let padding = left.extra_right + 1 + right.extra_left - interspace in
  let leaves, right_padding =
    if padding > 0 then
      match List.length leaves with
      | 0 -> ([], (1 + padding))
      | n ->
        let step = float padding /. float n in
        let pad = ref 0.0 in
        let leaves =
          List.map (fun leaf ->
              pad := !pad +. step;
              let leaf = {leaf with pad = 1 + int_of_float !pad} in
              pad := !pad -. float leaf.pad;
              leaf
            ) leaves
        in
        (leaves, if !pad > 0.0 then int_of_float (!pad +. 1.5) else 1)
    else
      (leaves, 1)
  in
  { extra_left = left.extra_left;
    nodes = (left.nodes @ leaves @ match right.nodes with
      | [] -> []
      | r :: rs -> {r with pad = r.pad + right_padding} :: rs);
    extra_right = right.extra_right
  }

let measure child =
  let pad_leaves xs = List.map (fun leaf -> {leaf with pad = 1}) xs in
  let leaves, row =
    List.fold_right (fun node (leaves, row) ->
        if node.child_width > 0 then
          let row' = of_node node in
          let row =
            match row with
            | None ->
              let width = width_of leaves + Int.max 0 (List.length leaves - 1) in
              {row' with nodes = row'.nodes @ pad_leaves leaves;
                         extra_right = Int.max 0 (row'.extra_right - width)}
            | Some row ->
              (join row' leaves row)
          in
          ([], Some row)
        else
          (node :: leaves, row)
      ) child ([], None)
  in
  let leaves = match leaves with
    | [] -> []
    | l :: ls -> l :: pad_leaves ls
  in
  let leaves_width = width_of leaves in
  match row with
  | None ->
    (0, leaves_width, 0, leaves)
  | Some row ->
    let nodes = match leaves, row.nodes with
      | (_ :: _), x :: xs -> {x with pad = x.pad + 1} :: xs
      | _, nodes -> nodes
    in
    (Int.max 0 (row.extra_left - leaves_width),
     leaves_width + width_of nodes,
     row.extra_right,
     nodes)

let node label child =
  let child_left, child_width, child_right, child = measure child in
  { label; width = String.length label; pad = 0;
    child_left; child_width; child_right; child }

module Padbuf : sig
  type t
  val make : unit -> t
  (*val buf : t -> Buffer.t*)
  val add : t -> string -> unit
  val add_unicode : t -> int -> string -> unit
  val pad : t -> int -> string -> unit
  (*val clear : t -> unit*)
  val flush : t -> string
end = struct
  type t = {
    buf: Buffer.t;
    mutable ofs: int;
  }

  let make () = {
    buf = Buffer.create 63;
    ofs = 0;
  }

  let clear t =
    Buffer.clear t.buf;
    t.ofs <- 0

  let column t =
    Buffer.length t.buf - t.ofs

  (*let buf t = t.buf*)

  let add_unicode t length raw =
    Buffer.add_string t.buf raw;
    t.ofs <- t.ofs + String.length raw - length

  let pad t col marker =
    while column t < col do
      add_unicode t 1 marker
    done

  let add t txt =
    Buffer.add_string t.buf txt

  let flush t =
    let result = Buffer.contents t.buf in
    clear t;
    result
end

type buffers = {
  text: Padbuf.t;
  table: Padbuf.t;
}


let add_node t (column, next) node =
  (* Print our label *)
  let column = column + node.pad in
  if node.label <> "" then (
    Padbuf.pad t.text column " ";
    Padbuf.add t.text node.label;
    Padbuf.pad t.text (column + node.width) " ";
  );
  let next =
    (* Print dashes for children and add children to the next row *)
    if node.child_width > 0 then (
      let delta = (node.child_width - node.width) / 2 in
      Padbuf.pad t.table (column - delta) " ";
      Padbuf.pad t.table (column + node.width / 2) "─";
      if node.label <> "" then
        Padbuf.add_unicode t.table 1 "┬";
      Padbuf.pad t.table (column - delta + node.child_width) "─";
      (column - delta, node.child) :: next
    ) else
      next
  in
  (column + node.width, next)

let layout_lines nodes =
  let buffers = {
    text = Padbuf.make ();
    table = Padbuf.make ();
  } in
  let left, _width, _right, sized = measure nodes in
  let print_group next (column, nodes) =
    let (_, next) = List.fold_left (add_node buffers) (column, next) nodes in
    next
  in
  let cons x xs = if x = "" then xs else x :: xs in
  let rec print_groups acc = function
    | [] -> acc
    | groups ->
      let next = List.fold_left print_group [] (List.rev groups) in
      let acc =
        cons (Padbuf.flush buffers.table) @@
        cons (Padbuf.flush buffers.text) @@
        acc
      in
      print_groups acc next
  in
  print_groups [] [left, sized]

let output oc nodes =
  List.iter
    (fun text -> output_string oc text; output_char oc '\n')
    (layout_lines nodes)
