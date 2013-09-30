module D = Debug.Make(struct let name = "Traces_stream" and default=`Debug end)
open D

(* Concrete execution of a streamed trace block *)
let concrete_stream mem_hash concrete_state thread_map block return =
  let open Traces in
  let block = Memory2array.coerce_prog_state mem_hash block in
  let memv = Memory2array.coerce_rvar_state mem_hash Asmir.x86_mem in
  let block = explicit_thread_stmts block thread_map in
  if return then
    run_block ~transformf:trace_transform_stmt concrete_state memv thread_map block
  else (
    ignore(run_block concrete_state memv thread_map block);
    []
  )

let concrete return =
    let mem_hash = Memory2array.create_state () in
    let concrete_state = Traces.TraceConcrete.create_state () in
    let thread_map = Traces.create_thread_map_state () in
    (* HACK to make sure default memory has a map to normalized memory *)
    ignore(Memory2array.coerce_rvar_state mem_hash Asmir.x86_mem);
    (fun block -> concrete_stream mem_hash concrete_state thread_map block return)

module MakeStreamSymbolic (TraceSymbolic:Traces.TraceSymbolic with type user_init = Traces.standard_user_init with type output = unit) =
struct

  let generate_formula_setup mem_hash concrete_state thread_map block =
    let block =
      concrete_stream mem_hash concrete_state thread_map block true
    in
    let block = Traces.remove_specials block in
    let block = Hacks.replace_unknowns block in
    block

  (* XXX: The implementation of this function is a little weird.
     We go to extreme lengths to make sure that last_state is not
     global.  It would be more elegant to have a general utility for
     keeping stack in streamtrans.ml, but I'm not sure how to do
     that. *)
  let generate_formula filename solver =
    let last_state = ref None in
    let mem_hash = Memory2array.create_state () in
    let concrete_state = Traces.TraceConcrete.create_state () in
    let thread_map = Traces.create_thread_map_state () in
    (* HACK to make sure default memory has a map to normalized memory *)
    ignore(Memory2array.coerce_rvar_state mem_hash Asmir.x86_mem);
    (* Streaming function *)
    (fun block ->
      let block = generate_formula_setup mem_hash concrete_state thread_map block in
      let state = match !last_state with
        | Some s -> s
        (* If this is the first block, make a new state *)
        | None ->
          (* Do we need to set dsa_rev_map? *)
          TraceSymbolic.create_state (filename,solver)
      in
      last_state :=
        Some (TraceSymbolic.symbolic_run_blocks state block)),

    (* Output formula *)
    (fun () ->
      match !last_state with
      | Some s -> TraceSymbolic.output_formula s
      | None -> failwith "Can not output formula for empty state!")
end

module StreamSymbolic =
  MakeStreamSymbolic(Traces.TraceSymbolicStream)

let generate_formula = StreamSymbolic.generate_formula
