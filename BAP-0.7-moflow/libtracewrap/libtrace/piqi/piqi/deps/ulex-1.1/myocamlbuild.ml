open Ocamlbuild_plugin;;
open Command;;
dispatch begin function
| After_rules ->
    flag ["ocaml"; "pp"; "use_ulex"] (S[A"camlp4o"; A"pa_ulex.cma"]);
    dep ["ocaml"; "ocamldep"; "use_ulex"] ["pa_ulex.cma"];
    ocaml_lib ~tag_name:"use_ulex" "ulexing";
| _ -> ()
end;;
