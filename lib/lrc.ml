(* MIT License
 *
 * Copyright (c) 2025 Frédéric Bour <frederic.bour@lakaban.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** This module constructs a graph refining the LR automaton to reason about
  reachable configurations---the pairs of an LR state and a lookahead token, the
  transitions that allow to go from one to another, in order to determine which
  ones are reachable from initial states. It includes functionality to compute
  reachable states, wait states, entry points, predecessors, successors, and
  prefixes for states in the LR automaton.
  LRC means "LR with classes", where a class is the partition of lookahead
  symbols with identical behaviors, as determined by the reachability
  analysis. *)

open Utils
open Misc
open Fix.Indexing

module type INDEXED = Info.INDEXED

(* Extended signature for class-based refinements *)
module type S = sig
  module Info : Info.S
  open Info

  (** Wait states lifted to LRC *)
  module LrC : sig
    include INDEXED
    val all_wait : set
    val lr1_of_lrc : t -> Lr1.t
    val lrcs_of_lr1 : Lr1.t -> set
    val first_lrc_of_lr1 : Lr1.t -> t
    val lookaheads : t -> Terminal.set
    val class_index : t -> int
    val to_string : t -> string
    val set_to_string : set -> string
  end

  module TrC : sig
    (* Abstract types used as index to represent the different sets of
       transitions.
       For instance, [goto] represents the finite set of goto transition:
       - the value [goto : goto cardinal] is the cardinal of this set
       - any value of type [goto index] is a member of this set
         (representing a goto transition)
    *)
    type goto
    type shift
    type any

    (* The set of goto transitions *)
    val goto : goto cardinal
    (* The set of all transitions = goto U shift *)
    val any : any cardinal
    (* The set of shift transitions *)
    val shift : shift cardinal

    (* Building the isomorphism between any and goto U shift *)

    (* Inject goto into any *)
    val of_goto : goto index -> any index

    (* Inject shift into any *)
    val of_shift : shift index -> any index

    (* Project a transition into a goto or a shift transition *)
    val split : any index -> (goto index, shift index) either

    (* A goto transition is a refinement of an LR(1) goto transition.
       (while shift transitions are the same as LR(1) shift transitions) *)
    val core : any index -> Transition.any index

    (* Get the source state of a transition *)
    val source : any index -> LrC.t

    (* Get the target state of a transition *)
    val target : any index -> LrC.t

    (* Symbol that labels a transition *)
    val symbol : any index -> Symbol.t

    (* Symbol that labels a goto transition *)
    val goto_symbol : goto index -> Nonterminal.t

    (* Symbol that labels a shift transition *)
    val shift_symbol : shift index -> Terminal.t

    (* [successors s] returns all the transitions [tr] such that
       [source tr = s] *)
    val successors : LrC.t -> any indexset

    (* [predecessors s] returns all the transitions [tr] such that
       [target tr = s] *)
    val predecessors : LrC.t -> any indexset

    (* Minimal number of symbols needed to take a transition *)
    val cost : any index -> int

    val accepting : goto indexset
  end

  module RedC : sig
    include INDEXED
    val path : t -> TrC.any index list
    val cost : t -> int
    val production : t -> Production.t
    val target : t -> TrC.goto index
    val lookaheads : t -> Terminal.set
    val from_target : TrC.goto index -> set
  end
end

(* Signature for a fully-featured LRC module with reachability information *)
module type FROM_ENTRYPOINTS = sig
  include S

  module Reachable : sig
    val lrc : LrC.set
    val wait : LrC.set
    val entrypoints : LrC.set
    val predecessors : LrC.t -> LrC.set
    val successors : LrC.t -> LrC.set
    val trc : TrC.any Boolvector.t
  end
end


(* Functor to create an LRC module from an Info module and Reachability module *)
module Make
    (I : Info.S)
    (Reachability : Reachability.S with module Info := I)
    ()
: S with module Info := I
=
struct
  open I

  (* Start timing for LRC computation *)
  let time = Stopwatch.enter Stopwatch.main "Lrc.Make"

  module LrC = struct
    let () = Stopwatch.step time "Construction of LrC set"

    (* Compute the total number of LRC states *)
    let n =
      let count lr1 = Array.length (Reachability.Classes.for_lr1 lr1) in
      let sum = ref 0 in
      Index.iter Lr1.n (fun lr1 -> sum := !sum + count lr1);
      !sum

    (* Lift `n` to type-level *)
    include Const(struct let cardinal = n end)

    type t = n index
    type set = n indexset
    type 'a map = (n, 'a) indexmap

    (* Shift an index by a given offset *)
    let index_shift (i : n index) offset =
      Index.of_int n ((i :> int) + offset)

    (* Compute the difference between two indices *)
    let index_delta (type n) (i : n index) (j : n index) =
      (i :> int) - (j :> int)

    (* Map from LRC states to their corresponding LR1 states *)
    let lr1_of_lrc = Vector.make' n (fun () -> Index.of_int Lr1.n 0)

    (* Map from LR1 states to their corresponding set of LRC states *)
    let lrcs_of_lr1 =
      let count = ref 0 in
      let init_lr1 lr1 =
        let classes = Reachability.Classes.for_lr1 lr1 in
        assert (Array.length classes > 0);
        let first = Index.of_int n !count in
        count := !count + Array.length classes;
        let all = ref IndexSet.empty in
        for i = Array.length classes - 1 downto 0 do
          let lrc = index_shift first i in
          all := IndexSet.add lrc !all;
          lr1_of_lrc.:(lrc) <- lr1
        done;
        !all
      in
      Vector.init Lr1.n init_lr1

    (* Map from LR1 states to their first LRC state *)
    let first_lrc_of_lr1 = Vector.map (fun x -> Option.get (IndexSet.minimum x)) lrcs_of_lr1

    (* Accessors for the mappings between LRC and LR1 states *)
    let lr1_of_lrc       = Vector.get lr1_of_lrc
    let lrcs_of_lr1      = Vector.get lrcs_of_lr1
    let first_lrc_of_lr1 = Vector.get first_lrc_of_lr1

    (* Set of wait LRC states *)
    let all_wait = IndexSet.map first_lrc_of_lr1 Lr1.wait

    (* Compute the class index for an LRC state *)
    let class_index lrc =
      let lr1 = lr1_of_lrc lrc in
      let lrc0 = first_lrc_of_lr1 lr1 in
      index_delta lrc lrc0

    (* Compute the lookahead terminals for an LRC state *)
    let lookaheads lrc =
      let lr1 = lr1_of_lrc lrc in
      let lrc0 = first_lrc_of_lr1 lr1 in
      let lookaheads = Reachability.Classes.for_lr1 lr1 in
      lookaheads.(index_delta lrc lrc0)

    (* Convert an LRC state to a string representation *)
    let to_string lrc =
      Printf.sprintf "%s/%d"
        (Lr1.to_string (lr1_of_lrc lrc))
        (class_index lrc)

    (* Convert a set of LRC states to a string representation *)
    let set_to_string lrcs =
      string_of_indexset ~index:to_string lrcs

    let () =
      let monotonous a =
          for i = 0 to Array.length a - 2
          do assert (IndexSet.compare_minimum a.(i) a.(i+1) < 0) done
      in
      Index.iter Lr1.n (fun lr1 -> monotonous (Reachability.Classes.for_lr1 lr1));
      Index.iter Transition.goto (fun gt -> monotonous (Reachability.Classes.for_edge gt))

    let () = Stopwatch.step time "Done with LrC, %d states (from %d Lr1 states)" (cardinal n) (cardinal Lr1.n)
  end

  module TrC = struct
   let () = Stopwatch.step time "Construction of TrC set"

   (* Reify shift transitions *)

   module Shift = Const(struct
       let cardinal =
         let count = ref 0 in
         Index.iter Transition.shift begin fun sh ->
           let target = Transition.target (Transition.of_shift sh) in
           count := !count + IndexSet.cardinal (LrC.lrcs_of_lr1 target)
         end;
         !count
     end)

   type shift = Shift.n
   let shift = Shift.n

   let shift_source = Vector.make' shift
       (fun () -> Index.of_int LrC.n 0)

   let shift_core = Vector.make' shift
       (fun () -> Index.of_int Transition.shift 0)

   let shift_target = Vector.make' shift
       (fun () -> Index.of_int LrC.n 0)

   let () =
     let enum = Index.enumerate shift in
     Index.iter Transition.shift @@ fun sh ->
     let tr = Transition.of_shift sh in
     let source =
       IndexSet.find
         (fun source ->
            IndexSet.mem (Transition.shift_symbol sh)
              (LrC.lookaheads source))
         (LrC.lrcs_of_lr1 (Transition.source tr))
     in
     IndexSet.iter (fun target ->
         let i = enum () in
         shift_source.:(i) <- source;
         shift_target.:(i) <- target;
         shift_core.:(i) <- sh
       ) (LrC.lrcs_of_lr1 (Transition.target tr))

   (* Reify goto transitions *)

   let goto_count = ref 0

   let goto_group = Vector.init Transition.goto @@ fun gt ->
     let tr = Transition.of_goto gt in
     let target = Transition.target tr in
     let pre_classes = Array.length (Reachability.Classes.pre_transition tr) in
     let posts = Reachability.Classes.for_edge gt in
     let outers = Reachability.Classes.for_lr1 target in
     let post_classes = Array.length posts in
     let outer_classes = Array.length outers in
     Array.iter (fun outer ->
         assert (Array.exists (fun s -> IndexSet.subset s outer) posts)
       ) outers;
     let coercion = Reachability.Coercion.infix posts outers in
     let costs = Reachability.Cells.table.:(Reachability.Tree.leaf tr) in
     Array.init pre_classes begin fun pre ->
       let outers = ref [] in
       for outer = outer_classes - 1 downto 0 do
         let post = coercion.backward.(outer) in
         let cost =
           if post = -1
           then max_int
           else
           try costs.(Reachability.Cells.table_index ~post_classes ~pre ~post)
           with Invalid_argument _ as exn ->
             Printf.eprintf "pre:%d post:%d post_classes:%d\n"
               pre post post_classes;
             raise exn
         in
         if cost < max_int then (
           incr goto_count;
           push outers (outer, cost)
         )
       done;
       !outers
     end

   module Goto = Const(struct let cardinal = !goto_count end)

   module Any = Sum(Goto)(Shift)
   type any = Any.n
   let any = Any.n
   type goto = Goto.n
   let goto = Goto.n

   let of_goto = Any.inj_l
   let of_shift = Any.inj_r
   let split = Any.prj

   let predecessors = Vector.make LrC.n IndexSet.empty
   let successors = Vector.make LrC.n IndexSet.empty

   (* Populate shift transitions *)
   let () = Index.rev_iter shift @@ fun sh ->
     let tr = of_shift sh in
     successors.@(shift_source.:(sh)) <- IndexSet.add tr;
     predecessors.@(shift_target.:(sh)) <- IndexSet.add tr

   (* Populate goto *)

   let goto_core = Vector.make' Goto.n (fun _ -> Index.of_int Transition.goto 0)
   let goto_pre = Vector.make Goto.n 0
   let goto_post = Vector.make Goto.n 0
   let goto_cost = Vector.make Goto.n 0

   let () =
     let enum = Index.rev_enumerate Goto.n in
     Vector.rev_iteri begin fun gt classes ->
       let tr = Transition.of_goto gt in
       let srcs = LrC.first_lrc_of_lr1 (Transition.source tr) in
       let tgts = LrC.first_lrc_of_lr1 (Transition.target tr) in
       Array.iteri begin fun pre post_classes ->
         List.iter begin fun (post, cost) ->
           let gt' = enum () in
           goto_core.:(gt') <- gt;
           goto_cost.:(gt') <- cost;
           goto_pre.:(gt') <- pre;
           goto_post.:(gt') <- post;
           let tr = of_goto gt' in
           successors.@(LrC.index_shift srcs pre) <- IndexSet.add tr;
           predecessors.@(LrC.index_shift tgts post) <- IndexSet.add tr;
         end post_classes
       end classes
     end goto_group

    let cost tr = match split tr with
      | L gt -> goto_cost.:(gt)
      | R _ -> 1

    let source tr = match split tr with
      | R sh -> shift_source.:(sh)
      | L gt ->
        LrC.index_shift
          (LrC.first_lrc_of_lr1 (Transition.source (Transition.of_goto goto_core.:(gt))))
          goto_pre.:(gt)

    let target tr = match split tr with
      | R sh -> shift_target.:(sh)
      | L gt ->
        LrC.index_shift
          (LrC.first_lrc_of_lr1 (Transition.target (Transition.of_goto goto_core.:(gt))))
          goto_post.:(gt)

    let goto_symbol gt = Transition.goto_symbol goto_core.:(gt)
    let shift_symbol sh = Transition.shift_symbol shift_core.:(sh)

    let symbol tr = match split tr with
      | L gt -> Symbol.inj_r (goto_symbol gt)
      | R sh -> Symbol.inj_l (shift_symbol sh)

    let successors = Vector.get successors
    let predecessors = Vector.get predecessors

    let accepting =
      IndexSet.init_from_set goto
        (fun gt -> IndexSet.mem goto_core.:(gt) Transition.accepting)

    let core tr = match split tr with
      | L gt -> Transition.of_goto goto_core.:(gt)
      | R sh -> Transition.of_shift shift_core.:(sh)

    let () = Stopwatch.step time "Done with TrC, %d transitions (from %d in Lr1), %d initial"
        (cardinal any) (cardinal Transition.any) (IndexSet.cardinal accepting)
  end

  module RedC = struct
    let () = Stopwatch.step time "Construction of RedC set"

    type desc = {
      path : TrC.any index list;
      cost : int;
      target : TrC.goto index;
      prod : Production.t;
      lookaheads : Terminal.set;
    }

    let _pts ts = string_of_indexset ~index:Terminal.to_string ts

    let () =
      Index.iter LrC.n (fun lrc ->
          let lr1 = LrC.lr1_of_lrc lrc in
          assert (IndexSet.mem lrc (LrC.lrcs_of_lr1 lr1));
          IndexSet.iter (fun tr ->
              assert (TrC.target tr = lrc)
            ) (TrC.predecessors lrc);
          IndexSet.iter (fun tr ->
              assert (TrC.source tr = lrc)
            ) (TrC.successors lrc);
        )

    let acc = ref []

    let rec simulate_reduction lookaheads prod position cost path state =
      if position > 0 then
        IndexSet.iter (fun tr ->
            simulate_reduction lookaheads prod
              (position - 1)
              (cost + TrC.cost tr)
              (tr :: path)
              (TrC.source tr)
          ) (TrC.predecessors state)
      else
        IndexSet.iter (fun tr ->
            match TrC.split tr with
            | R _ -> ()
            | L gt ->
              if Index.equal (TrC.goto_symbol gt) (Production.lhs prod) then (
                let lookaheads =
                  IndexSet.inter lookaheads (LrC.lookaheads (TrC.target tr))
                in
                if not (IndexSet.is_empty lookaheads) then
                  push acc {path; cost; target=gt; prod; lookaheads};
              )
          ) (TrC.successors state)

    let () =
      Index.iter LrC.n @@ fun lrc ->
      let lookaheads = LrC.lookaheads lrc in
      IndexSet.iter (fun red ->
          let lookaheads =
            Terminal.intersect (Reduction.lookaheads red) lookaheads
          in
          let prod = Reduction.production red in
          if not (IndexSet.is_empty lookaheads) then
            simulate_reduction
              lookaheads prod (Production.length prod) 0 [] lrc
        ) (Reduction.from_lr1 (LrC.lr1_of_lrc lrc))

    include Vector.Of_array(struct type a = desc let array = Array.of_list !acc end)

    type t = n index
    type set = n indexset
    type 'a map = (n, 'a) indexmap
    let n = Vector.length vector

    let path i = vector.:(i).path
    let cost i = vector.:(i).cost
    let production i = vector.:(i).prod
    let target i = vector.:(i).target
    let lookaheads i = vector.:(i).lookaheads

    let from_target =
      let table = Vector.make TrC.goto IndexSet.empty in
      let register i t = table.@(t.target) <- IndexSet.add i in
      Vector.rev_iteri register vector;
      Vector.get table
    let () = Stopwatch.step time "Done with RedC, %d reduction scenario\n\
                                  (%.02f ways to take a transition on average)"
        (cardinal n) (float (cardinal n) /. float (cardinal TrC.goto))
  end

  (* End timing after computing all necessary information *)
  let () = Stopwatch.leave time
end
