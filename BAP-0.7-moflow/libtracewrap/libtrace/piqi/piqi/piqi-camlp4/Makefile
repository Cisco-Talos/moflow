include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqi-camlp4
PIQI_LIB_NAME = syntax


INCDIRS = +camlp4
PRE_TARGETS = \
	pa_labelscope.ml pa_openin.ml \
	pa_labelscope.cmo pa_labelscope.cmi \
	pa_openin.cmo pa_openin.cmi


LIBINSTALL_FILES = $(PRE_TARGETS)


# latest officially supported OCAML version
LATEST_SUPPORTED_OCAML = 4.00

OCAML_VERSION ?= $(shell ocamlfind ocamlc -version | sed -e 's/^\([0-9]*\.[0-9]*\)\..*/\1/')


all: $(OCAML_VERSION) $(PRE_TARGETS)


pre_target::
	@echo "Building CamlP4 extensions for OCaml $(OCAML_VERSION)"


post_target:: install


$(OCAML_VERSION):
	@echo "There is no library for OCaml $(OCAML_VERSION), trying $(LATEST_SUPPORTED_OCAML) ..."
	$(MAKE) OCAML_VERSION=$(LATEST_SUPPORTED_OCAML) all


#NOTE: when installing, uninstall first to avoid "already installed" error
install: uninstall libinstall
	$(PIQI_ROOT)/make/piqi-fix-libinstall $(RESULT) $(PIQI_LIB_NAME)

uninstall: libuninstall


%.ml: $(OCAML_VERSION)/%.ml
	ln -s $< $@


include $(OCAMLMAKEFILE)
