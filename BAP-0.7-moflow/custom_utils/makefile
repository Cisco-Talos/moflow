BAP_DIR = ..

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
OCAMLFLAGS=$(INCLUDES)  -g   -ccopt -rdynamic -ccopt -lrt -ccopt -L$(BAP_LIB_DIR) -ccopt -L$(LIBASMIR)
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

SLICER_OBJS=common.cmo slicer.cmo
motriage_OBJS=common.cmo motriage.cmo
TRACEME_OBJS=traceme.o

all: slicer motriage

slicer: $(SLICER_OBJS)
	export OCAMLPATH=$(PIQIBUILDDIR); \
	ocamlfind ocamlc -package $(PACKS) -linkpkg -thread $(OCAMLFLAGS) $(LIBS) -o slicer $(SLICER_OBJS) 

motriage: $(motriage_OBJS)
	export OCAMLPATH=$(PIQIBUILDDIR); \
	ocamlfind ocamlc -package $(PACKS) -linkpkg -thread $(OCAMLFLAGS) $(LIBS) -o motriage $(motriage_OBJS) 

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
	rm -f slicer motriage
	rm -f *.cm[iox] *.o

# Dependencies
.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > $@

include .depend

