# this build relies on OCamlMakefile build script, it can be found here:
#
#	http://www.ocaml.info/home/ocaml_sources.html#ocaml-make
#

RESULT = config


SOURCES = \
	$(PIQI_ML_FILES) \
	config.ml


PACKS = piqi.lib


PIQI_FILES = config.piqi
PIQI_ML_FILES = config_piqi.ml config_piqi_ext.ml


PRE_TARGETS = $(PIQI_ML_FILES)


PIQIC = piqic
PIQIC_FLAGS = --pp --leave-tmp-files


all: native-code #byte-code debug-code


$(PIQI_ML_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) ocaml-ext $(PIQIC_FLAGS) $$i ; \
	done


clean::
	rm -f *.tmp.ml


include OCamlMakefile
