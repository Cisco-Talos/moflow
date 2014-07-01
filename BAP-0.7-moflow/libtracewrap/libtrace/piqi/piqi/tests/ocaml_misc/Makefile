include $(PIQI_ROOT)/make/Makefile.ocaml

RESULT = test


SOURCES = $(PIQI_ML_FILES)


PIQI_FILES = \
	empty-record.piqi \
	\
	Ad.piqi P.piqi Protocol.piqi \
	\
	variant-1.piqi variant-2.piqi


PIQI_ML_FILES = \
	empty_record_piqi.ml \
	\
	ad.ml p.ml protocol.ml \
	\
	variant_1_piqi.ml variant_2_piqi.ml


PRE_TARGETS = $(PIQI_ML_FILES)


PIQIC = piqic ocaml
PIQIC_FLAGS = --pp --gen-defaults


PACKS = piqi.runtime

# LIBS = piqirun
# LIBDIRS = $(PIQI_ROOT)/piqirun-ocaml
# INCDIRS = $(PIQI_ROOT)/piqirun-ocaml


all: nc #top


$(PIQI_ML_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i ; \
	done



include $(OCAMLMAKEFILE)
