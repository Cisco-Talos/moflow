include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml
include $(PIQI_ROOT)/piqilib/Makefile.piqi_common


RESULT = piqic


SOURCES += \
	piqic_common.ml \
	\
	piqic_ocaml_types.ml \
	piqic_ocaml_out.ml \
	piqic_ocaml_in.ml \
	piqic_ocaml_defaults.ml \
	piqic_ocaml.ml \
	piqic_ocaml_ext.ml \
	\
	piqic_erlang_types.ml \
	piqic_erlang_out.ml \
	piqic_erlang_in.ml \
	piqic_erlang_defaults.ml \
	piqic_erlang.ml \
	\
	piqic_expand.ml \
	\
	piqic_run.ml \


PRE_TARGETS += $(PIQI_SOURCES) 


PIQICC = ../piqicc/piqicc
PIQIC_FLAGS = -I $(PIQI_ROOT)
PIQICC_FLAGS = $(PIQIC_FLAGS)


all: nc

debug: dc top


piqi_lang_piqi.ml: $(wildcard *.piqi)
	$(PIQICC) $(PIQICC_FLAGS) -o $@.tmp.ml \
		--spec piqi.piqi --lang piqi-lang.piqi --impl piqi-impl.piqi
	$(CAMLP4_O) -o $@ $@.tmp.ml


clean::
	rm -f *.tmp.ml


include $(OCAMLMAKEFILE)
