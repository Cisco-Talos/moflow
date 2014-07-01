include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml
include $(PIQI_ROOT)/piqilib/Makefile.piqi_common


RESULT = piqicc


PIQIC_LIB_SOURCES = \
	piqic_common.ml \
	piqic_ocaml_types.ml \
	piqic_ocaml_out.ml \
	piqic_ocaml_in.ml \
	piqic_ocaml_defaults.ml \
	piqic_ocaml.ml \


SOURCES += $(PIQIC_LIB_SOURCES)
SOURCES += piqicc.ml piqicc_run.ml


PRE_TARGETS += $(PIQI_SOURCES) $(PIQIC_LIB_SOURCES)


PIQIC_FLAGS = -I $(PIQI_ROOT)
PIQICC_FLAGS = $(PIQIC_FLAGS)


all: dc top #bc


$(PIQIC_LIB_SOURCES): %.ml: $(PIQI_ROOT)/piqic/%.ml
	ln -s $< $@


piqi_lang_piqi.ml: boot/piqi_lang_piqi.ml
	cp $< $@


.PHONY: reboot


reboot:
	./piqicc $(PIQICC_FLAGS) -o piqi_lang_piqi.tmp.ml \
		--spec piqi.piqi --lang piqi-lang.piqi --impl piqi-impl.piqi
	$(CAMLP4_O) -o boot/piqi_lang_piqi.ml piqi_lang_piqi.tmp.ml
	rm -f piqi_lang_piqi.tmp.ml


clean::
	rm -f piqi_lang_piqi.tmp.ml


include $(OCAMLMAKEFILE)

