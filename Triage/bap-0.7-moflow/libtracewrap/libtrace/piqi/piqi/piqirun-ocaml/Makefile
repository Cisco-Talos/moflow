include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqirun
PIQI_LIB_NAME = runtime


SOURCES = piqirun.ml


LIBINSTALL_FILES = \
	$(RESULT).cmi \
	$(RESULT).cma $(RESULT).cmxa \
	$(RESULT).$(EXT_LIB) \
	# $(RESULT).mli -- TODO


STATIC = 1  # build a static library (don't build dlls)

# LIBS =
# LIBDIRS = 
# INCDIRS =
# RESULTDEPS =

# PACKS = 
# ANNOTATE = yes
# INCDIRS = +camlp4
# USE_CAMLP4 = yes


all: dcl ncl test #top


post_target:: install


#NOTE: when installing, uninstall first to avoid "already installed" error
install: uninstall libinstall
	$(PIQI_ROOT)/make/piqi-fix-libinstall $(RESULT) $(PIQI_LIB_NAME)

uninstall: libuninstall


test:
	ocaml piqirun.cma test.ml


# these commands are useful for debugging:
#ocamlc -c -g piqirun.cma test.ml 
#ocaml -init test.ocaml piqirun.cma test.cmo 
#
#ocamlc -g piqirun.cma test.ml 
#OCAMLRUNPARAM=b ./a.out


clean::
	rm -f test.cm? a.out


include $(OCAMLMAKEFILE)
