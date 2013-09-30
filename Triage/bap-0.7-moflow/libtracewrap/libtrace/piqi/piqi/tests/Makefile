include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.dirs


# Are Protocol Buffers tools installed? Assuming here that the C++ .so libraries
# are installed as well.
ifneq ($(shell which protoc), )
DIRS_PROTO = \
       piqi_proto \
       piqi_proto1 \
       piqi_packed \
       boot_proto2piqi \
       cpp
endif


DIRS_ERLANG = \
	erlang_addressbook \
	erlang_piqi \
	erlang_perf \
	erlang_packed \
	erlang_misc \
	erlang_import \


DIRS_OCAML = \
       ocaml_addressbook \
       ocaml_piqi \
       ocaml_perf \
       ocaml_packed \
       ocaml_array \
       ocaml_misc \
       ocaml_custom_types \
       ocaml_piq_config \
       ocaml_riak_pb \


DIRS = \
       examples \
       self_spec \
       piqi_misc \
       piqi_server \
       piqi_compile \
       $(DIRS_OCAML) \
       $(DIRS_PROTO)


.PHONY: ocaml ocaml-clean erlang erlang-clean proto proto-clean


proto:
	set -e; \
	for i in $(DIRS_PROTO); do \
		$(MAKE) -C $$i; \
	done


proto-clean:
	for i in $(DIRS_PROTO); do \
		$(MAKE) -C $$i clean; \
	done


ocaml:
	set -e; \
	for i in $(DIRS_OCAML); do \
		$(MAKE) -C $$i; \
	done


ocaml-clean:
	for i in $(DIRS_OCAML); do \
		$(MAKE) -C $$i clean; \
	done


erlang:
	set -e; \
	for i in $(DIRS_ERLANG); do \
		$(MAKE) -C $$i; \
	done


erlang-clean:
	for i in $(DIRS_ERLANG); do \
		$(MAKE) -C $$i clean; \
	done


clean:: erlang-clean

