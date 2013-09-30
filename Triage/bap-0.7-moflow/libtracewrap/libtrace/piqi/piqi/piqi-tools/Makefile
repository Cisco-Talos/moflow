include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqi


PACKS = piqi.lib unix

SOURCES = \
	piqi_json_pp.ml \
	\
	piqi_pp_cmd.ml \
	piqi_convert_cmd.ml \
	piqi_check.ml \
	piqi_expand.ml \
	piqi_light_cmd.ml \
	piqi_getopt_cmd.ml \
	\
	$(PIQI_ML_SOURCES) \
	\
	piqi_to_proto.ml \
	piqi_of_proto.ml \
	\
	piqi_http.ml \
	piqi_rpc.ml \
	piqi_server.ml \
	piqi_call.ml \
	\
	piqi_compile.ml \
	piqi_cc.ml \
	\
	piqi_run.ml \


# make sure piqi gets rebuilt when we change the library
RESULTDEPS = ../piqilib/piqilib.cmxa $(wildcard ../piqilib/piqilib.cma)


PIQI_MODULES = descriptor.proto.piqi piqi-tools.piqi piqi-rpc.piqi

PIQI_ML_SOURCES = descriptor_piqi.ml piqi_tools_piqi.ml piqi_rpc_piqi.ml


PRE_TARGETS += \
	$(PIQI_SOURCES) $(PIQI_PIQI) $(PIQI_ML_SOURCES)
	

PIQIC = ../piqic/piqic
# TODO: we don't need to embed piqi in descriptor.ml
PIQIC_FLAGS = -I $(PIQI_ROOT) --embed-piqi


#OCAMLBLDFLAGS = -custom


all: nc


debug: dc top


$(PIQI_ML_SOURCES): $(PIQI_MODULES)
	set -e; \
	for i in $^; \
	do \
		$(PIQIC) ocaml $(PIQIC_FLAGS) --pp $$i; \
	done


# rule for re-creating descriptor.proto.piqi from a (newer) descriptor.proto
# (it is called manually when when there are changes in the upstream
# descriptor.proto)
descriptor: descriptor.proto
	./piqi of-proto --normalize $<


.PHONY: descriptor


clean::


include $(OCAMLMAKEFILE)
