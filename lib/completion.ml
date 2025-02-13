open Fix.Indexing
open Utils

(* Construct the LRC Completion automaton *)

let (.@()<-) v i f = Vector.set v i (f (Vector.get v i))

module Make(Info : Info.S)(Lrc: Lrc.S with module Info := Info) =
struct
  open Info

  (* Find all the reachable reduction paths and items that go through an Lrc
     state. *)
  type reduction_path = {
    suffix: Lrc.n index list;
    position: int;
    production: Production.t;
    lookaheads: Terminal.set;
  }

  let count = ref 0
  let paths = Vector.make Lrc.n []

  let () =
    let rec simulate_reduction path state =
      incr count;
      Vector.set_cons paths state path;
      if path.position > 0 then
        let path' = {
          suffix = state :: path.suffix;
          position = path.position - 1;
          production = path.production;
          lookaheads = path.lookaheads;
        } in
        IndexSet.iter
          (simulate_reduction path')
          (Lrc.predecessors state)
    in
    Index.iter Lrc.n (fun lrc ->
        let lr1 = Lrc.lr1_of_lrc lrc in
        let lookaheads = Lrc.lookahead lrc in
        IndexSet.iter (fun red ->
            let lookaheads =
              Terminal.intersect (Reduction.lookaheads red) lookaheads
            in
            if not (IndexSet.is_empty lookaheads) then (
              let production = Reduction.production red in
              let path = {
                suffix = [];
                position = Production.length production;
                production;
                lookaheads;
              } in
              simulate_reduction path lrc
            )
          ) (Reduction.from_lr1 lr1)
      )

  let () =
    Printf.eprintf "Found %d reachable reduction paths\n" !count
  (*type state =
    | Suffix of {
        goto: Lrc.n list;
        top: Lrc.n;
        lookaheads: Terminal.set;
      }
    | Reduce of {
        goto: Lrc.n list;
        top: Lrc.n;
        lookaheads: Terminal.set;
        lhs: Nonterminal.n;
        dot: int;
      }*)
end
