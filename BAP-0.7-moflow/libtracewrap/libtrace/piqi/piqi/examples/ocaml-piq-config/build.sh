
set -ex
piqic ocaml-ext --pp config.piqi
ocamlfind ocamlopt -package piqi.lib -c config_piqi.ml
ocamlfind ocamlopt -package piqi.lib -c config_piqi_ext.ml
ocamlfind ocamlopt -package piqi.lib -c config.ml
ocamlfind ocamlopt \
				-package piqi.lib -linkpkg \
				                -o config \
				config_piqi.cmx config_piqi_ext.cmx config.cmx
