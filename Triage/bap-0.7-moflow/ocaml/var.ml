(**
    The type used for variables and functions to create and use them.
 *)

module D = Debug.Make(struct let name="Var" and default=`Debug end)
open D

module V = struct

  type t = V of int * string * Type.typ


  let hash (V(i,_,_)) = i

  let equal = ( == )

  (* is it faster to use the generic compare, or < and = ? *)
  let compare (V(x,_,_)) (V(y,_,_)) = compare x y
end

include V

module VHMake = Hashtbl.Make(V)

module VarHash = struct
  include VHMake
  (* Overwrite broken replace:
     http://caml.inria.fr/mantis/view.php?id=5349 *)
  include Util.HashUtil(VHMake)
end
module VarMap = BatMap.Make(V)
module VarSet = BatSet.Make(V)



let newvar =
  let varcounter = ref 0 in
  (fun s t ->
     let n = !varcounter in
     if n = -1 then failwith "newvar: counter wrapped around";
     (varcounter := n+1;
      V(n,s,t))
  )


let renewvar (V(_,name,t)) = newvar name t

let typ (V(_,_,t)) = t

let name (V(_,n,_)) = n
