ifeq ($(MAKECMDGOALS),distclean)
-include Makefile.config
-include $(PIQI_ROOT)/make/Makefile.dirs
else
include Makefile.config
include $(PIQI_ROOT)/make/Makefile.dirs
endif


DIRS = \
	piqi-camlp4 \
	piqirun-ocaml \
	piqicc piqic piqilib piqi-tools \


.PHONY: deps build-dir install distclean \
	ocaml ocaml-install ocaml-uninstall \
	erlang erlang-clean


# export installation and search path for OCaml dependencies
ifeq ($(MAKECMDGOALS),deps)
OCAMLFIND_DESTDIR = $(PIQI_BUILD)/lib/ocaml
OCAMLPATH = $(OCAMLFIND_DESTDIR)
export OCAMLFIND_DESTDIR OCAMLPATH
endif


# export installation path for third-party deps and Piqi OCaml libraries
ifneq ($(findstring ocaml-,$(MAKECMDGOALS)),)
ifneq ($(PIQI_OCAML_DESTDIR),)
OCAMLFIND_DESTDIR = $(PIQI_OCAML_DESTDIR)
OCAMLPATH = $(OCAMLFIND_DESTDIR)
export OCAMLFIND_DESTDIR OCAMLPATH
endif
endif


pre_target:: build-dir


deps: build-dir
	$(MAKE) -C deps
	$(MAKE) -C deps install


build-dir:
	mkdir -p $(PIQI_BUILD)/lib/ocaml


install:
	-install -d $(PIQI_PREFIX)/bin
	install piqi-tools/piqi $(PIQI_PREFIX)/bin
	install piqic/piqic $(PIQI_PREFIX)/bin


ocaml:
	$(MAKE) -C piqilib bcl install


ocaml-install: ocaml-uninstall
	test -d $(PIQI_OCAML_DESTDIR) || mkdir -p $(PIQI_OCAML_DESTDIR)
	ocamlfind install piqi `ls $(PIQI_BUILD)/lib/ocaml/piqi/*`
	$(MAKE) -C deps install


ocaml-uninstall:
	ocamlfind remove piqi
	$(MAKE) -C deps uninstall


erlang:
	$(MAKE) -C piqi-erlang


erlang-clean:
	$(MAKE) -C piqi-erlang clean
	$(MAKE) -C piqi-rpc clean


clean:: erlang-clean
	$(MAKE) -C deps clean
	$(MAKE) -C tests clean


distclean:
	if [ -f Makefile.config ]; then \
		$(MAKE) clean; \
		rm -rf $(PIQI_BUILD); \
		rm Makefile.config; \
	fi

