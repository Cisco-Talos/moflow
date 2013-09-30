(** Statically identify obvious use of system call numbers on AST CFGs.

    Finds uses like [mov $20, %eax. sysenter] that a compiler would
    emit.

    Assumes that [%eax] contains the system call number before the
    system call is executed.

    @author ejs
*)

(** find_syscalls [cfg] returns a list of tuples [(bb, n)]. A tuple
    [(bb, n)] in the output list denotes that {e all} executions
    reaching [bb] perform system call [n].

    The non-existance of a tuple [(bb, n)] does not imply that an
    execution reaching [bb] will {e not} perform system call [n]; it
    only means that the static analysis was unable to prove {e all}
    executions reaching [bb] will perform system call [n].
*)
val find_syscalls : Cfg.AST.G.t -> (Cfg.AST.G.V.t * Big_int_Z.big_int list) list
