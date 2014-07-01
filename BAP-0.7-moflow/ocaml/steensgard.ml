(* Steensgard's loop nesting algorithm

   See Steensgaard, B. (1993). Sequentializing Program Dependence
   Graphs for Irreducible Programs (No. MSR-TR-93-14), particularly
   page 5.  (The algorithm is very simple)
*)

module D = Debug.Make(struct let name = "Steensgard" and default=`NoDebug end)
open D

module type G =
sig
  include Graph.Builder.S

  val remove_edge_e : G.t -> G.E.t -> G.t
  val v2s : G.V.t -> string
end

type 'a loopinfo =
  | BB of 'a
  | Other of 'a loopinfo list
  | Loop of 'a loopinfo list (* body of loop *)

module Make(C:G) =
struct

  (* Get loop information from Steensgard's loop forest *)
  let steensgard cfg =
    let module Comp = Graph.Components.Make(C.G) in
    let module VS = Set.Make(C.G.V) in
    let f cfg =
      let sccs = Comp.scc_list cfg in
      let rec process_scc cfg scc =
        dprintf "process_scc";
        match scc with
        | [] -> failwith "loopinfo_from_steensgard: impossible"
        | [x] when C.G.mem_edge cfg x x -> dprintf "Self loop at %s" (C.v2s x);
          Loop [BB(x)]
        | [x] -> dprintf "BB %s" (C.v2s x); BB x
        | _ ->
          let h = Hashtbl.create (List.length scc) in
          List.iter (fun v -> dprintf "scc %s" (C.v2s v); Hashtbl.add h v ()) scc;
          let cfg = cfg in

          let entry_nodes = C.G.fold_edges_e (fun e s ->
            if Hashtbl.mem h (C.G.E.dst e) = true && Hashtbl.mem h (C.G.E.src e) = false
            then (dprintf "entry %s" (C.v2s (C.G.E.dst e)); VS.add (C.G.E.dst e) s)
            else s
          ) cfg VS.empty in

          let closing_edges = C.G.fold_edges_e (fun e l ->
            if Hashtbl.mem h (C.G.E.src e) = true && VS.mem (C.G.E.dst e) entry_nodes = true
            then e::l
            else l
          ) cfg [] in

          dprintf "entry nodes %d closing edges %d" (VS.cardinal entry_nodes) (List.length closing_edges);

          (* Progress check *)
          assert (closing_edges <> []);

          (* Remove closing edges *)
          let cfg = List.fold_left C.remove_edge_e cfg closing_edges in

          (* SCCs contained in original region *)
          let sccs = List.filter (fun scc ->
            List.for_all (Hashtbl.mem h) scc
          ) (Comp.scc_list cfg) in

          Loop(List.map (process_scc cfg) sccs)

      in
      match sccs with
      | [] -> failwith "loopinfo_from_steensgard: impossible"
      | [x] -> process_scc cfg x
      | _ -> Other(List.map (process_scc cfg) sccs)
    in
    f cfg

end

let steensgard_ast =
  let module S = Make(Cfg.AST) in
  S.steensgard

let steensgard_ssa =
  let module S = Make(Cfg.SSA) in
  S.steensgard
