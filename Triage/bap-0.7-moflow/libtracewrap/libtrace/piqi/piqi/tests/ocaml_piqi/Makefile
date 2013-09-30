all: prep test


prep:
	cat ../piqi_misc/piqobj.piqi | sed -e 's/piqi\///' > piqobj.piqi
	piqi convert --add-defaults -t pb piqi.piqi


test:
	$(MAKE) -f Makefile.ocaml
	./otest
	cmp *.pb


clean:
	$(MAKE) -f Makefile.ocaml clean
	rm -f piqobj.piqi piqi.piqi.pb* t.*

