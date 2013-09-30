(**

   A module with IL models of system calls.

*)

open Ast

(**

   We are going to index each system call model by the value of eax.
   Because this is going to be quite time-consuming we will use (for
   now) specials for system call models that are unimplemented.

*)

let x86_is_system_call = function
  | Special(("int 80"|"syscall"), _) -> true
  | _ -> false

(* System call names - fill in as needed *)
let linux_get_name = function
  | 1 -> "exit"
  | 3 -> "read"
  | 4 -> "write"
  | 5 -> "open"
  | 6 -> "close"
  | 33 -> "access"
  | 45 -> "brk"
  | 54 -> "ioctl"
  | 91 -> "munmap"
  | 102 -> "socketcall"
  | 122 -> "uname"
  | 125 -> "mprotect"
  | 146 -> "writev"
  | 175 -> "sigprocmask"
  | 192 -> "mmap2"
  | 195 -> "stat64"
  | 197 -> "fstat64"
  | 199 -> "getuid32"
  | 200 -> "getgid32"
  | 201 -> "geteuid32"
  | 202 -> "getegid32"
  | 221 -> "fcntl64"
  | 224 -> "gettid"
  | 240 -> "futex"
  | 243 -> "set_thread_area"
  | 252 -> "exit_group"
  | 270 -> "tgkill"
  | n -> "unknown syscall #" ^ string_of_int n

(* Fill in system call models as needed *)
let linux_get_model = function
  | 1 ->
      (* exit *)
      (* Exit code is in ebx *)
    Some(Halt(Var Disasm_i386.ebx, [])
         :: [])
  | 252 ->
    (* exit group *)
    Some(Halt(Var Disasm_i386.ebx, [])
         :: [])
  | _ ->
      None

let linux_syscall_to_il eax =
  match linux_get_model eax with
    | Some model ->
      Comment((linux_get_name eax) ^ " model", [])
      :: model
    | None ->
        let sys_name = linux_get_name eax in
        Special (sys_name, [])
        :: Move(Disasm_i386.eax, Unknown("System call output", reg_32), [])
        :: []
