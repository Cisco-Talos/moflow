include ../../Makefile.config

ERL_SOURCES = \
	piqi_rpc_runtime.erl \
	\
	piqi_rpc.erl \
	piqi_rpc_app.erl \
	piqi_rpc_sup.erl \
	\
	piqi_rpc_monitor.erl \
	\
	piqi_rpc_http.erl \
	piqi_rpc_webmachine_resource.erl \
	\
	piqic_erlang_rpc.erl


ERL_TESTS = piqi_rpc_webmachine_resource


ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
ERLC_FLAGS = #-pa $(PIQI_ROOT)/piqi-erlang #-I $(PIQI_ROOT)/piqi-erlang

#EBIN_DIR = .


VSN = $(shell cat $(PIQI_ROOT)/VERSION)
ERL_APP_FILE = piqi_rpc.app


all: ebin


test: beams
	for i in $(ERL_TESTS); do \
	    $(ERL) -pa $(EBIN_DIR) -noshell -s eunit test $$i -s init stop; \
	done


$(ERL_APP_FILE): $(ERL_APP_FILE).src
	sed -e 's/git/"$(VSN)"/' $< > $@


clean::
	rm -f $(ERL_APP_FILE)


include $(PIQI_ROOT)/make/Makefile.erlang

