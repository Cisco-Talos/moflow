.NOTPARALLEL:

all : lib tests
lib : objsize.cma objsize.cmxa

VER=0.16
DIST=objsize-$(VER).tar.gz
FILES=alloc.c bitarray.c c_objsize.c Makefile objsize.ml \
      objsize.mli tests.ml util.h README ocamlsrc META

.PHONY : all dist tests clean tests-installed

WIN=0

ifneq ($(windir)$(WINDIR),)
WIN=1
endif
ifeq ($(OSTYPE),msys)
WIN=1
endif

ifeq ($(WIN),0)
RUN=./
SO=so
else
RUN=.\\
SO=dll
endif

dist : $(DIST)

tests : tests.nat.exe tests.byt.exe
	./tests.nat.exe
	CAML_LD_LIBRARY_PATH=. ./tests.byt.exe

tests-installed : clean tests-inst.byt.exe tests-inst.nat.exe

objsize.cma : c_objsize.o objsize.cmo
	ocamlmklib -o objsize -oc objsize -linkall $^

objsize.cmxa: c_objsize.o objsize.cmx
	ocamlmklib -o objsize -oc objsize -linkall $^

c_objsize.o : c_objsize.c bitarray.c alloc.c
	ocamlc -ccopt -W -ccopt -Wall -c -I ./ocamlsrc/byterun c_objsize.c

objsize.cmi : objsize.mli
	ocamlc -c $<

objsize.cmo : objsize.ml objsize.cmi
	ocamlc -c $<

objsize.cmx : objsize.ml objsize.cmi
	ocamlopt -c $<

tests.cmo : tests.ml objsize.cmi
	ocamlc -pp camlp4r -c $<

tests.cmx : tests.ml objsize.cmi
	ocamlopt -pp camlp4r -c $<

tests.byt.exe : tests.cmo objsize.cma
	ocamlc -cclib -Wl,--stack=0x1000000 -cclib -L. \
	  objsize.cma tests.cmo -o $@

tests.nat.exe : tests.cmx objsize.cmxa
	ocamlopt -cclib -L. objsize.cmxa tests.cmx -o $@

tests-inst.byt.exe : tests.ml
	ocamlfind ocamlc -package objsize -linkpkg \
	  -pp camlp4r \
	  -cclib -Wl,--stack=0x1000000 \
	  tests.ml -o $@

tests-inst.nat.exe : tests.ml
	ocamlfind ocamlopt -package objsize -linkpkg \
	  -pp camlp4r \
	  tests.ml -o $@

DISTDIRBASE=objsize-$(VER)
DISTDIR=../$(DISTDIRBASE)

$(DIST) : $(FILES)
	$(MAKE) clean  &&  \
	 if [ -d "$(DISTDIR)" ]; \
	 then \
	   ( echo can\'t make distfile: there already exists directory $(DISTDIR); \
	     exit 1 ); \
	 else \
	   ( mkdir "$(DISTDIR)"; cp -R $(FILES) "$(DISTDIR)"; \
	     (cd .. && tar -cf - "$(DISTDIRBASE)" && rm -rf "$(DISTDIRBASE)" ) | \
	     gzip -9 > "$(DIST)" ); \
	 fi

clean :
	rm -f objsize.a objsize.cmi \
	  objsize.*cm* objsize.o dllobjsize.* \
	  libobjsize.a \
	  tests.nat.exe tests.byt.exe c_objsize.o \
	  tests.o tests.*cm* $(DIST) \
	  configure.o \
	  tests-inst.byt.exe tests-inst.nat.exe

install : objsize.cma objsize.cmxa
	ocamlfind install -patch-version $(VER) objsize META \
	  libobjsize.a objsize.a objsize.cma \
	  objsize.cmi objsize.cmxa objsize.mli \
	  -dll dllobjsize.$(SO)

uninstall :
	ocamlfind remove objsize

tags:
	-