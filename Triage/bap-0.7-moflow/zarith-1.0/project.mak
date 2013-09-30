# This file is part of the Zarith library 
# http://forge.ocamlcore.org/projects/zarith .
# It is distributed under LGPL 2 licensing, with static linking exception.
# See the LICENSE file included in the distribution.
#   
# Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
# Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
# a joint laboratory by:
# CNRS (Centre national de la recherche scientifique, France),
# ENS (École normale supérieure, Paris, France),
# INRIA Rocquencourt (Institut national de recherche en informatique, France).


# project files
###############

CSRC = caml_z.c
SSRC = $(wildcard caml_z_$(ARCH).S)
MLSRC = z.ml q.ml big_int_Z.ml
MLISRC = z.mli q.mli big_int_Z.mli

AUTOGEN = z.ml z.mli

CMIOBJ = $(MLISRC:%.mli=%.cmi)
TOINSTALL = zarith.a zarith.cma zarith.cmxa zarith.cmxs libzarith.a $(MLISRC) $(CMIOBJ)


# build targets
###############

all: $(TOINSTALL) test

zarith.cma: $(MLSRC:%.ml=%.cmo)
	$(OCAMLMKLIB) -o zarith $+ $(LIBS)

zarith.cmxa zarith.a: $(MLSRC:%.ml=%.cmx)
	$(OCAMLMKLIB) -o zarith $+ $(LIBS)

zarith.cmxs: zarith.cmxa libzarith.a
	$(OCAMLOPT) -shared -o $@ -I . zarith.cmxa

libzarith.a dllzarith.so: $(SSRC:%.S=%.o) $(CSRC:%.c=%.o) 
	$(OCAMLMKLIB) -o zarith $+ $(LIBS)

test: zarith.cmxa test.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -cclib "-L." $+ -o $@

test.b: zarith.cma test.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -cclib "-L." $+ -o $@

rtest: zarith.cmxa rtest.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) gmp.cmxa bigarray.cmxa -cclib "-L." $+ -o $@

bitest: zarith.cmxa  bitest.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) nums.cmxa -cclib "-L." $+ -o $@

doc: $(MLISRC)
	mkdir -p html
	$(OCAMLDOC) -html -d html -charset utf8 $+



# install targets
#################

ifeq ($(INSTMETH),install)
install:
	install -d $(INSTALLDIR)
	for i in $(TOINSTALL); do \
		if test -f $$i; then $(INSTALL) -m 0644 $$i $(INSTALLDIR); fi; \
	done
	if test -f dllzarith.so; then $(INSTALL) -m 0755 dllzarith.so $(INSTALLDIR)/stublibs; fi

uninstall:
	for i in $(TOINSTALL); do \
		rm -f $(INSTALLDIR)/$$i; \
	done
	if test -f $(INSTALLDIR)/stublibs/dllzarith.so; then rm -f $(INSTALLDIR)/stublibs/dllzarith.so; fi
endif

ifeq ($(INSTMETH),findlib)
install:
	$(OCAMLFIND) install -destdir $(INSTALLDIR) zarith META $(TOINSTALL) dllzarith.so

uninstall:
	$(OCAMLFIND) remove -destdir $(INSTALLDIR) zarith
endif


# rules
#######

$(AUTOGEN): z.mlp z.mlip $(SSRC) z_pp.pl
	./z_pp.pl $(ARCH)

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<

%.o: %.c
	$(OCAMLC) -verbose -ccopt "$(CFLAGS)" -c $<

clean:
	/bin/rm -rf *.o *.a *.so *.cmi *.cmo *.cmx *.cmxa *.cmxs *.cma *.dll *~ \#* depend test $(AUTOGEN) tmp.c depend
	/bin/rm -rf test test.b rtest bitest html

depend: $(AUTOGEN)
	$(OCAMLDEP) -native $(OCAMLINC) *.ml *.mli > depend

include depend

.PHONY: clean
