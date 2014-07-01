
all:
	cat ../piqi_packed/packed.piqi | sed -e 's/\.protobuf-packed//' > unpacked.piqi
	cat ../piqi_packed/test-all.piq | sed -e 's/:packed/:unpacked/' > test-all-unpacked.piq

	$(MAKE) -f Makefile.ocaml

	piqi convert -t pb test-all.piq
	./test
	cmp test-all.piq.pb test-all.piq.pb.packed
	cmp test-all.piq.pb test-all.piq.pb.packed-array

	cp test-all.piq.pb test-all.piq.pb.orig
	piqi convert -t pb -o test-all.piq.pb test-all-unpacked.piq
	./test
	cmp test-all.piq.pb.orig test-all.piq.pb.packed
	cmp test-all.piq.pb.orig test-all.piq.pb.packed-array


clean:
	$(MAKE) -f Makefile.ocaml clean
	rm -f test-all.piq.* unpacked.piqi test-all-unpacked.piq
