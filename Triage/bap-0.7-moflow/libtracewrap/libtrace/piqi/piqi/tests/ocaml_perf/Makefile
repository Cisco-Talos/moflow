.PHONY: all ocaml test clean


all: ocaml test


ocaml: piqi-obj.piqi
	$(MAKE) -f Makefile.ocaml


piqi-obj.piqi: ../../piqi/piqi.piqi
	ln -sf ../../piqic/piqi.ocaml.piqi
	piqi expand -e ocaml piqi.piqi > $@


test: ocaml
	piqi convert -t pb addressbook.piq

	echo ":piqi-obj/piqi [" > piqi.piq
	cat piqi-obj.piqi >> piqi.piq
	echo "]" >> piqi.piq

	piqi convert --no-warnings --add-defaults -t pb piqi.piq
	#./test


clean:
	$(MAKE) -f Makefile.ocaml clean
	rm -f addressbook.piq.pb piqi-expanded.piqi piqi-obj.* piqi.ocaml.piqi piqi.piq piqi.piq.pb

