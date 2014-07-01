.PHONY: all erlang test erlang_ext test_ex clean


all: erlang test erlang_ext test_ext


erlang:
	$(MAKE) -f Makefile.erlang


test: erlang
	./test


erlang_ext:
	rm -f addressbook_piqi.beam # forcing make to rebuild it
	$(MAKE) -f Makefile.erlang_ext


test_ext: erlang_ext
	./io_json_xml_pb addressbook.piq.pb


clean:
	$(MAKE) -f Makefile.erlang clean
	rm -f addressbook.piq.pb addressbook.pb.piq l1 l2
	$(MAKE) -f Makefile.erlang_ext clean

