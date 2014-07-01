BAP_DIR = ../..

BAP_LIB_DIR = $(BAP_DIR)/ocaml/
LIBASMIR=$(BAP_DIR)/libasmir/src/
BIGINT=$(BAP_DIR)/zarith-1.0
BATT=$(BAP_DIR)/batteries/_build/src
OUNIT=$(BAP_DIR)/ounit/_build/src/
PCRE=$(BAP_DIR)/pcre-ocaml/lib
OCAMLGRAPH=$(BAP_DIR)/ocamlgraph-1.8
OBJSIZE=$(BAP_DIR)/objsize-0.16
LIBTRACE=$(BAP_DIR)/libtracewrap/libtrace/src/ocaml
PIQIBUILDDIR=$(BAP_DIR)/libtracewrap/libtrace/piqi/piqi/build/lib/ocaml

INC = $(BIGINT) $(BATT) $(OUNIT) $(PCRE) $(OCAMLGRAPH) $(OBJSIZE) $(LIBTRACE) $(LIBTRACE)/piqi $(BAP_LIB_DIR)
PACKS = bigarray,str,ocamlgraph,unix,camomile,threads,num,piqi.lib
LIBS = zarith.cma batteries.cma objsize.cma oUnit.cma graph.cma pcre.cma libtrace.cma bap.cma

OCAMLC=ocamlc
CC=gcc
CFLAGS=-Wall -m32
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=$(foreach d, $(INC), -I $d)
OCAMLFLAGS=$(INCLUDES)  -g   -ccopt -rdynamic -ccopt -L$(BAP_LIB_DIR) -ccopt -L$(LIBASMIR) -thread
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

egas_OBJS=stats.cmo heap.cmo common.cmo export.cmo branches.cmo space.cmo mylazy.cmo oracle.cmo apps.cmo egas.cmo 

all: egas

egas: $(egas_OBJS)
	export OCAMLPATH=$(PIQIBUILDDIR); \
	ocamlfind ocamlc -package $(PACKS) -linkpkg -thread $(OCAMLFLAGS) $(LIBS) -o egas $(egas_OBJS) 

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .o

.ml.cmo: 
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f egas
	rm -f *.cm[iox] *.o
	rm -f depend

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > $@

include depend

