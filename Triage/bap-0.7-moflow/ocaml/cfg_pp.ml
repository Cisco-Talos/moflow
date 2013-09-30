(** Pretty printing for CFGs. *)


module CS = Cfg.SSA
module CA = Cfg.AST

module type DOTTYG =
sig
  type t 
  module V : Graph.Sig.COMPARABLE
  module E :
  sig
    type t
    type label
    val label : t -> label
    val src : t -> V.t
    val dst : t -> V.t
  end


  val iter_vertex : (V.t -> unit) -> t -> unit

  val iter_edges_e : (E.t -> unit) -> t -> unit
  val graph_attributes : t -> Graph.Graphviz.DotAttributes.graph list
  val default_vertex_attributes : t -> Graph.Graphviz.DotAttributes.vertex list
  val vertex_name : V.t -> string
  val vertex_attributes : V.t -> Graph.Graphviz.DotAttributes.vertex list
  val get_subgraph : V.t -> Graph.Graphviz.DotAttributes.subgraph option
  val default_edge_attributes : t -> Graph.Graphviz.DotAttributes.edge list
  val edge_attributes : E.t -> Graph.Graphviz.DotAttributes.edge list
end


(* Just for convenience *)
module DefAttrs =
struct
  let graph_attributes _ = []
  let default_vertex_attributes _ = [`Shape `Box]
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end

module DefAttributor =
struct
  let vertex_attributes _ _ = []
  and edge_attributes _ _ = []
end

(* (\* FIXME: Instead of having two of these we should take the graph *)
(*    module and type f accordingly *\) *)
(* module FunSsaAttributor = *)
(* struct *)
(*   let f = ref (fun g v -> raise Not_found) *)
(*   include DefAttributor *)
(*   let vertex_attributes (g:'a) (v:'b) = try !f g v with Not_found -> [] *)
(* end *)

(* module FunAstAttributor = *)
(* struct *)
(*   let f = ref (fun g v -> raise Not_found) *)
(*   include DefAttributor *)
(*   let vertex_attributes (g:'a) (v:'b) = try !f g v with Not_found -> [] *)
(* end *)

let edge_labels f e =
  match f e with
  | Some true -> [`Label "t"]
  | Some false -> [`Label "f"]
  | None -> []

let edge_labels_ssa = edge_labels CS.G.E.label
let edge_labels_ast = edge_labels CA.G.E.label


(** Makes a module suitable for use with Graph.Graphviz.Dot  for writting out
    a CFG. *)
module MakeCfgPrinter
  (G:Graph.Sig.G with type V.label = Cfg.bbid and type E.label = bool option)
  (Printer:sig val print: G.t -> G.V.t -> string end)
  (Attributor:sig val vertex_attributes: G.t -> G.V.t -> Graph.Graphviz.DotAttributes.vertex list ;;
                  val edge_attributes: G.t -> G.E.t -> Graph.Graphviz.DotAttributes.edge list end)
  : (DOTTYG with type t = G.t and type V.t = G.V.t * G.t and type E.t =  G.E.t * G.t)
  =
struct
  type t = G.t

  module V =
  struct
    type t = G.V.t * G.t
    let hash (v,g) = G.V.hash v
    let equal x y = G.V.equal (fst x) (fst y)
    let compare x y = G.V.compare (fst x) (fst y)
  end
  module E =
  struct 
    type t = G.E.t * G.t
    type label = G.E.label
    let label (e,g) = G.E.label e
    let src (e,g) = (G.E.src e, g)
    let dst (e,g) = (G.E.dst e, g)
  end

  let iter_edges_e f g =
    G.iter_edges_e (fun e -> f (e,g)) g

  let iter_vertex f g =
    G.iter_vertex (fun v -> f (v,g)) g

  include DefAttrs

  let printer = ref (fun _ -> failwith "Uninitialized printer")

  let graph_attributes g =
    (* Use this as an initialization routine *)
    printer := Printer.print g;
    []

  let vertex_name (v,g) = Cfg.bbid_to_string (G.V.label v)

  let vertex_attributes (v,g) =
    (* FIXME: The Dot module really should be the one doing the escaping here *)
    `Label (String.escaped(!printer v)) :: Attributor.vertex_attributes g v

  let edge_attributes ((e,g) as e') = (edge_labels E.label e') @ Attributor.edge_attributes g e

end



module PrintSsaStmts =
struct
  let print g =
    let buf = Buffer.create 1000 in
    let ft = Format.formatter_of_buffer buf in
    let pp = new Pp.pp ft in
    let pr = Buffer.add_string buf in
    (fun b ->
    let stmts = CS.get_stmts g b in
    pr(Cfg.bbid_to_string (CS.G.V.label b));
    pr "\n";
    pp#ssa_stmts stmts;
    Format.pp_print_flush ft ();
    let o = Buffer.contents buf in
    Buffer.clear buf;
    o)

  let edge_attributes = edge_labels_ssa
end

module PrintAstStmts =
struct
  let print g =
    let buf = Buffer.create 1000 in
    let ft = Format.formatter_of_buffer buf in
    let pp = new Pp.pp ft in
    let pr = Buffer.add_string buf in
    (fun b ->
    let stmts = CA.get_stmts g b in
    pr(Cfg.bbid_to_string (CA.G.V.label b));
    pr "\n";
    pp#ast_program stmts;
    Format.pp_print_flush ft ();
    let o = Buffer.contents buf in
    Buffer.clear buf;
    o)

  let edge_attributes = edge_labels_ast
end

module PrintAstAsms =
struct

  exception Found of string

  let append olds news =
    if olds = "" then news
    else olds ^ "\n" ^ news

  let print g b =
    let open Type in
    let stmts = CA.get_stmts g b in
    let out = List.fold_left (fun s stmt -> match stmt with
    | Ast.Label(Addr a, attrs) ->
      let addrstr = Printf.sprintf "%#Lx" a in
      let newasmsstr = 
        try let newasms = BatList.find_map
              (function
                | Asm asm -> Some asm
                | _ -> None) attrs in
            newasms
        with Not_found -> "Unknown" in
      append s (addrstr ^ ": " ^ newasmsstr)
    | _ -> s) "" stmts in
    match out with
    | "" -> Cfg.bbid_to_string(CA.G.V.label b)
    | _ -> out

  let edge_attributes = edge_labels_ast
end

module SsaStmtsPrinter = MakeCfgPrinter (CS.G) (PrintSsaStmts) (DefAttributor)
module SsaStmtsDot = Graph.Graphviz.Dot(SsaStmtsPrinter)

module AstStmtsPrinter = MakeCfgPrinter (CA.G) (PrintAstStmts) (DefAttributor)
module AstStmtsDot = Graph.Graphviz.Dot (AstStmtsPrinter)

module AstAsmsPrinter = MakeCfgPrinter (CA.G) (PrintAstAsms) (DefAttributor)
module AstAsmsDot = Graph.Graphviz.Dot (AstAsmsPrinter)

module SsaBBidPrinter =
struct
  include CS.G
  include DefAttrs
  let vertex_name v = Cfg.bbid_to_string(CS.G.V.label v)
  let edge_attributes = edge_labels_ssa
end
module SsaBBidDot = Graph.Graphviz.Dot(SsaBBidPrinter)

module AstBBidPrinter =
struct
  include CA.G
  include DefAttrs
  let vertex_name v = Cfg.bbid_to_string(CA.G.V.label v)
  let edge_attributes = edge_labels_ast
end
module AstBBidDot = Graph.Graphviz.Dot(AstBBidPrinter)

module SsaStmtsAttPrinter = MakeCfgPrinter (CS.G) (PrintSsaStmts) (DefAttributor)
module SsaStmtsAttDot = Graph.Graphviz.Dot(SsaStmtsAttPrinter)

module AstStmtsAttPrinter = MakeCfgPrinter (CA.G) (PrintAstStmts) (DefAttributor)
module AstStmtsAttDot = Graph.Graphviz.Dot(AstStmtsAttPrinter)
