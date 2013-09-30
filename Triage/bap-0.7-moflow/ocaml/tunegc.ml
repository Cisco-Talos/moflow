(* Make the garbage collector less aggressive, since we often have
   very large objects *)
let set_gc () =
  Gc.set
    {
      (Gc.get ()) with
	Gc.minor_heap_size = 32000000; (* 128 mb *)
	Gc.major_heap_increment = 16000000; (* 64 mb *)
	Gc.max_overhead = 100; (* compact after 100% overhead *)
    };;
