VERSION = 1.0.0
export VERSION

.PHONY: default all opt test doc soft-clean clean
default: all opt
all:
	ocamlc -c easy_format.mli
	ocamlc -c -dtypes easy_format.ml
	touch bytecode
opt:
	ocamlc -c easy_format.mli
	ocamlopt -c -dtypes easy_format.ml
	touch nativecode
test: all simple_example.out
	ocamlc -o test_easy_format -dtypes easy_format.cmo test_easy_format.ml
	./test_easy_format > test_easy_format.out
	ocamlc -o lambda_example -dtypes easy_format.cmo lambda_example.ml
	./lambda_example > lambda_example.out

simple_example: all simple_example.ml
	ocamlc -o simple_example -dtypes easy_format.cmo simple_example.ml
simple_example.out: simple_example
	./simple_example > simple_example.out

doc: ocamldoc/index.html easy_format_example.html
ocamldoc/index.html: easy_format.mli
	ocamldoc -d ocamldoc -html $<
easy_format_example.html: simple_example.out simple_example.ml
	cat simple_example.ml > easy_format_example.ml
	echo '(* Output: ' >> easy_format_example.ml
	cat simple_example.out >> easy_format_example.ml
	echo '*)' >> easy_format_example.ml
	ocamlc -c -dtypes easy_format_example.ml
	caml2html easy_format_example.ml -t -o easy_format_example.html

soft-clean:
	rm -f *.cm[iox] *.o *.annot \
		test_easy_format lambda_example simple_example \
		bytecode nativecode

clean: soft-clean
	rm -f *.out ocamldoc/* \
		easy_format_example.*
	cd examples; $(MAKE) clean


COMMON_INSTALL_FILES = META easy_format.cmi easy_format.mli
BC_INSTALL_FILES = easy_format.cmo 
NC_INSTALL_FILES = easy_format.cmx easy_format.o

install:
	echo "version = \"$(VERSION)\"" > META; cat META.tpl >> META
	INSTALL_FILES="$(COMMON_INSTALL_FILES)"; \
		if test -f bytecode; then \
		  INSTALL_FILES="$$INSTALL_FILES $(BC_INSTALL_FILES)"; \
		fi; \
		if test -f nativecode; then \
		  INSTALL_FILES="$$INSTALL_FILES $(NC_INSTALL_FILES)"; \
		fi; \
		ocamlfind install easy-format $$INSTALL_FILES

uninstall:
	ocamlfind remove easy-format

archive:
	@echo "Making archive for version $(VERSION)"
	$(MAKE) doc
	rm -rf /tmp/easy-format /tmp/easy-format-$(VERSION) && \
	 	cp -r . /tmp/easy-format && \
		cd /tmp/easy-format && \
			$(MAKE) clean && \
			rm -f *~ examples/*~ easy-format*.tar* && \
		cd /tmp && cp -r easy-format easy-format-$(VERSION) && \
		tar czf easy-format.tar.gz easy-format && \
		tar cjf easy-format.tar.bz2 easy-format && \
		tar czf easy-format-$(VERSION).tar.gz easy-format-$(VERSION) && \
		tar cjf easy-format-$(VERSION).tar.bz2 easy-format-$(VERSION)
	mv /tmp/easy-format.tar.gz /tmp/easy-format.tar.bz2 .
	mv /tmp/easy-format-$(VERSION).tar.gz /tmp/easy-format-$(VERSION).tar.bz2 .
	cp easy-format.tar.gz easy-format.tar.bz2 $$WWW/
	cp easy-format-$(VERSION).tar.gz easy-format-$(VERSION).tar.bz2 $$WWW/
	cp LICENSE $$WWW/easy-format-license.txt
	cp Changes $$WWW/easy-format-changes.txt
	cp easy_format_example.ml easy_format_example.html $$WWW/
	cp ocamldoc/* $$WWW/easy-format-doc/
	echo 'let easy_format_version = "$(VERSION)"' \
		> $$WWW/easy-format-version.ml
