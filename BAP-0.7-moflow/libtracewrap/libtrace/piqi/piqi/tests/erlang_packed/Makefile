
all:
	cat ../piqi_packed/packed.piqi | sed -e 's/\.protobuf-packed//' > unpacked.piqi
	cat ../piqi_packed/test-all.piq | sed -e 's/:packed/:unpacked/' > test-all-unpacked.piq

	$(MAKE) -f Makefile.erlang

	piqi convert -t pb test-all.piq
	./test
	cmp test-all.piq.pb test-all.piq.pb.pb

	cp test-all.piq.pb test-all.piq.pb.orig
	piqi convert -t pb -o test-all.piq.pb test-all-unpacked.piq
	./test
	cmp test-all.piq.pb.orig test-all.piq.pb.pb


clean:
	$(MAKE) -f Makefile.erlang clean
	rm -f test-all.piq.* unpacked.piqi test-all-unpacked.piq
