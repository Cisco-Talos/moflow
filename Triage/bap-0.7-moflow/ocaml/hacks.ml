(** Hacks *)

open Ast
open Ast_convenience
open Ast_visitor
open BatListFull
open Type
open Util

module C = Cfg.AST
module D = Debug.Make(struct let name = "Hacks" and default=`NoDebug end)
open D

let ra_final = Var.newvar "ra_final" reg_32
and ra0 = Var.newvar "ra0" Ast.reg_32
and (mem,sp,r_of) =
  let d = Asmir.decls_for_arch Asmir.arch_i386 in
  (List.hd d,
   List.find (fun v -> Var.name v = "R_ESP") d,
   List.find (fun v -> Var.name v = "R_OF") d
  )

let function_end = "function_end"
and attrs = [StrAttr "ret hack"]
let save_ra0 = Move(ra0, Load(Var mem, Var sp, exp_false, reg_32), attrs)

let ret_to_jmp ?(ra=ra_final) p =
  let a = Array.of_list p in
  Array.iteri
    (fun i s -> match s with
      (* Old lifting of ret *)
     | Special("ret", _) ->
	 a.(i) <- Jmp(Lab function_end, attrs);
	 (match a.(i-1) with 
	  | Jmp(t,at) -> a.(i-1) <- Move(ra, t, attrs@at)
	  | _ -> failwith "expected Jmp before ret special"
	 )
     (* disasm_i386 lifting of ret *)
     | Jmp(t, attrs) when attrs = [StrAttr "ret"] ->
       a.(i) <- Jmp(Lab function_end, attrs)
     | _ -> ()
    ) a;
  let l = Array.to_list a in
  save_ra0::l@[Label(Name function_end, attrs)]


let attrs = [StrAttr "noof hack"]
let assert_noof p =
  let il = List.map
    (function
       | Move(v, e, a) as s when v == r_of ->
	   [s; Assert(exp_not (Var v), attrs)]
       | s -> [s]
    ) p
  in
  List.flatten il

type color = White | Gray | Black

let remove_cycles cfg =
  let module C = Cfg.AST in
  let a = StrAttr "added by remove_cycles" in
  (* let assert_false = Assert(exp_false, a) in *)
  let assert_false = Jmp(Lab("BB_Error"), a::[]) in
  let cfg, error = Cfg_ast.find_error cfg in
  let handle_backedge cfg e =
    let s = C.G.E.src e in
    let l = C.G.E.label e in
    let revstmts = List.rev (C.get_stmts cfg s) in
    let revstmts = match revstmts with
      | Jmp(t,_)::rest ->
	  assert_false::rest
      | CJmp(c,t1,t2,attrs)::rest ->
	(* e is the label we are REMOVING *)
	(match l with
	| Some true -> CJmp(c, Lab("BB_Error"), t2, a::attrs)
	| Some false -> CJmp(c, t1, Lab("BB_Error"), a::attrs)
	| None -> failwith "missing edge label from cjmp")
          ::rest
      | rest -> assert_false::rest
    in
    let cfg = C.set_stmts cfg s (List.rev revstmts) in
    let cfg = C.remove_edge_e cfg e in
    let newedge = C.G.E.create s l (C.G.V.create Cfg.BB_Error) in
    let cfg = C.add_edge_e cfg newedge in
    cfg
  in
  let find_backedges cfg =
    let module H = Hashtbl.Make(Cfg.BBid) in
    let h = H.create (C.G.nb_vertex cfg)
    and entry = C.find_vertex cfg Cfg.BB_Entry in
    let color v = try H.find h (C.G.V.label v) with Not_found -> White
    and setcolor v c = H.replace h (C.G.V.label v) c in
    let rec walk v edges=
      let walk_edge e edges =
	let d = C.G.E.dst e in
	if color d = White then walk d edges
	else if color d = Gray then e::edges
        else edges
      in
      setcolor v Gray;
      let edges = C.G.fold_succ_e walk_edge cfg v edges in
      setcolor v Black;
      edges
    in
    walk entry []
  in
  dprintf "Removing backedges";
  let backedges = find_backedges cfg in
  (* I don't think we need this anymore... *)
  (* let backedges = Util.list_unique backedges in *)
  List.fold_left handle_backedge cfg backedges

(** Fix outgoing edges of [n] in [g] *)
let repair_node g n =
  let g, error = Cfg_ast.find_error g in
  let g, exit = Cfg_ast.find_exit g in
  let g, indirect = Cfg_ast.find_indirect g in
  (* Some of this is copied from Cfg_ast.of_prog *)
  let make_edge g v ?lab t =
    let dst = lab_of_exp t in
    let tgt = match dst with
      | None -> indirect
      | Some l ->
	  try (C.find_label g l)
	  with Not_found ->
	    wprintf "Jumping to unknown label: %s" (Pp.label_to_string l);
	    error
      (* FIXME: should jumping to an unknown address be an error or indirect? *)
    in
    C.add_edge_e g (C.G.E.create v lab tgt)
  in
  let edges = C.G.succ_e g n in
  let g = List.fold_left C.remove_edge_e g edges in
  let stmts = C.get_stmts g n in
  match List.rev stmts with
  | Jmp(t, _)::_ -> make_edge g n t
  | CJmp(_,t,f,_)::_ -> let g = make_edge g n ~lab:true t in
                     make_edge g n ~lab:false f
  | Special _::_ -> C.add_edge g n error
  | Halt _::_ -> C.add_edge g n exit
  | _ -> (* It's probably fine.  That's why this is in hacks.ml. *) g

(** Repair cfg whose graph is inconsistent with its statements *)
let repair_cfg g =
  (* XXX: Better implementation *)
  let p = Cfg_ast.to_prog g in
  (* let oc = open_out "p.out" in *)
  (* let pp = new Pp.pp_oc oc in *)
  (* pp#ast_program p; *)
  (* pp#close; *)
  let cfg = Cfg_ast.of_prog p in
  let cfg, entry = Cfg_ast.find_entry cfg in
  Reachable.AST.remove_unreachable cfg entry

(** Rename labels so they are always unique.  This is useful for traces. *)
let uniqueify_labels p =
  let lh = Hashtbl.create 1000 in
  let find_new_label l =
    let strl = match l with
      | Name(s) -> s
      | Addr(a) -> Printf.sprintf "addr_%Lx" a
    in
    let n =
      try (Hashtbl.find lh strl)+1
      with Not_found -> 0 
    in
    Hashtbl.replace lh strl n;
        (* Keep the first name unique to make sure cjmptrace labels
           don't get broken *)
    let newname =
      if n = 0 then strl else
        (strl^"_unique_"^(string_of_int n))
    in
    Name(newname)
  in
  let renamelabels = object(self)
    inherit Ast_visitor.nop
    method visit_label l = ChangeTo (find_new_label l)
  end
  in
  Ast_visitor.prog_accept renamelabels p

module Rm(C: Cfg.CFG) = struct
  let remove_indirect g =
    C.remove_vertex g (C.G.V.create Cfg.BB_Indirect)
end

let ast_remove_indirect =
  let module Rm = Rm(Cfg.AST) in
  Rm.remove_indirect

let ssa_remove_indirect =
  let module Rm = Rm(Cfg.SSA) in
  Rm.remove_indirect

(** Replace unknown expressions with constant zero *)
let replace_unknowns p =
  let i t = Int(Big_int_convenience.bi0, t) in
  let v = object(self)
    inherit Ast_visitor.nop
    method visit_exp = function
      | Unknown(_, t) ->
        ChangeTo (i t)
      | _ -> DoChildren
  end
  in
  Ast_visitor.prog_accept v p

(** Append src to dst *)
let append_file src dst =
  let oc1 = open_out_gen [Open_text; Open_append] 0o640 dst in
  let oc2 = open_in src in
  let rec do_append oc_out oc_in =
    try 
      output_string oc_out ((input_line oc_in)^"\n");
      do_append oc_out oc_in
    with End_of_file -> ()
  in
  do_append oc1 oc2;
  close_out oc1;
  close_in oc2

(** Add an "assume false" statement to BB_Error and add an edge to
    BB_Exit.

    Useful for testing validity with a bounded number of loops.
*)
let bberror_assume_false graph =
  let graph, error = Cfg_ast.find_error graph in
  let graph, exit = Cfg_ast.find_exit graph in
  let preds = C.G.pred graph error in
  let assume_label = "AssumeFalse" in
  let assume_stmts =
    [
      Label(Name assume_label, []);
      Assume(exp_false, [StrAttr "The program does not start"])
    ]
  in
  let graph, assume = C.create_vertex graph assume_stmts in
  let replace_label = function
    | Lab "BB_Error" -> ChangeTo (Lab assume_label)
    | _ -> DoChildren
  in
  let reroute cfg node =
    let stmts = C.get_stmts cfg node in
    let stmts = (Ast_mapper.map_e replace_label)#prog stmts in
    let cfg = C.set_stmts cfg node stmts in
    (*let cfg = C.remove_edge cfg node error in*)
    let cfg = C.add_edge cfg node assume in
    cfg
  in
  let graph = C.remove_vertex graph error in
  let graph = List.fold_left reroute graph preds in
  let graph = C.add_edge graph assume exit in
  graph
