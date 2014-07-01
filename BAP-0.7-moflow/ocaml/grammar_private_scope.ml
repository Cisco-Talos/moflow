(** Define a Scope solely for the Parser and its helper functions. *)

open Grammar_scope

let default_scope () = create_scope_from_decls Asmir.all_regs
let cur_scope : Scope.t ref = ref (default_scope ())

let get_scope () = !cur_scope
let set_scope s = cur_scope := s
let reset_scope () = set_scope (default_scope ())

(* This is really terrible, but I'm not sure how else to fix it.  We
   should have access to the standard x86/ARM registers when we parse.
   But this means that our Scope module has to rely on Disasm.  We also
   need Memory2array to rely on the Scope, but Memory2array has to be
   defined pretty early.  As a compromise, I am going to have Grammar
   (e.g. this file) grab the x86/ARM decls and put them into the default
   scope. It's ugly, and feel free to fix it if you can do better. *)

