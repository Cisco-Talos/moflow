open BatPervasives
open OUnit

module G = Graph.Pack.Digraph
module D = Dominator.Make(G)

let n = G.V.create;;

let n1 = n 1
and n2 = n 2
and n3 = n 3
and n4 = n 4
and n5 = n 5
and n6 = n 6

let build_graph () =
  let g = G.create () in
  List.iter
    (fun (s,d) -> G.add_edge g s d)
     ((n1,n2)
      :: (n1,n3)
      :: (n3,n4)
      :: (n2,n5)
      :: (n4,n5)
      :: []);
  g, n1

let dominees_test () =
  let g, entry = build_graph () in
  let dom_functions = D.compute_all g entry in
  let dominees = dom_functions.D.dominees in
  let list_eq l1 l2 node = assert_equal
    ~msg:("Dominees failed on " ^ node)
    (List.sort G.V.compare l1) (List.sort G.V.compare l2) in
  list_eq (dominees n1) [n1; n2; n3; n4; n5] "Node 1";
  list_eq (dominees n2) [n2] "Node 2";
  list_eq (dominees n3) [n3; n4] "Node 3";
  list_eq (dominees n4) [n4] "Node 4";
  list_eq (dominees n5) [n5] "Node 5"
;;

let suite = "Dominator" >:::
  [
    "dominees_test" >:: dominees_test
  ]
