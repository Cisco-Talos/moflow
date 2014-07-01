module L = List

module Attribs = 
struct
  type attr = Color of int | Label of string 
end

module Node = struct
 include Attribs
 type t = int * attr list
 let compare (v1,_) (v2,_) = Pervasives.compare v1 v2
 let hash (v,_) = Hashtbl.hash v
 let equal x y = (fst x) = (fst y)
end

(* representation of an edge -- must be comparable *)
module Edge = struct
 type t = string
 let compare = Pervasives.compare
 let equal = (=)
 let default = ""

 type label = string
 let create vs lbl vd = (vs, lbl, vd)
 let label e = e
end

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Dot(struct
  include G (* use the graph module from above *)
  include Attribs

  let cast_attrs l = 
    let cast = function
      | Color(c) -> `Color(c)
      | Label(s) -> `Label(s)
    in
    L.map cast l

  (* Light blue for edges. *)
  let edge_attributes (a, e, b) = [`Label e; `Color 0x99ccff]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes (v,attrs) = 
    let attrs = cast_attrs attrs in
    attrs @ [`Style `Filled; `Fontname "Courier";]

  let vertex_name (v,_) = string_of_int v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

module DotExporter =
struct
  include Attribs

  let init () = G.empty

  let add_node g node_v true_v false_v = 
    let et = Edge.create node_v "t" true_v in
    let ef = Edge.create node_v "f" false_v in
    let g = G.add_edge_e g et in
    let g = G.add_edge_e g ef in
    g

  let save_to_file g ofn = 
    let file = open_out_bin ofn in
    let () = Dot.output_graph file g in
    close_out file

end
