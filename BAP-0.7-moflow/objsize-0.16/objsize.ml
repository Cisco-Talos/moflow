if Sys.ocaml_version < "3.11"
then
  failwith "Objsize >=0.12 can only be used with OCaml >=3.11"

type info =
  { data : int
  ; headers : int
  ; depth : int
  }

external internal_objsize : unit -> Obj.t -> info = "ml_objsize"

let objsize obj = internal_objsize () (Obj.repr obj)

let size_with_headers i = (Sys.word_size/8) * (i.data + i.headers)

let size_without_headers i = (Sys.word_size/8) * i.data
