
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	process_info_example.erl


ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
ERLC_FLAGS = #-pa $(PIQI_ROOT)/piqi-erlang #-I $(PIQI_ROOT)/piqi-erlang

EBIN_DIR = .


PIQIC = piqic-erlang-rpc


PIQI_FILE = process_info.piqi


PIQI_ERL_MOD = process_info


PIQI_HRL_FILES = $(PIQI_ERL_MOD)_piqi.hrl $(PIQI_ERL_MOD)_piqi_impl.hrl
PIQI_ERL_FILES = $(PIQI_ERL_MOD)_piqi.erl \
		 $(PIQI_ERL_MOD)_piqi_default_impl.erl \
		 $(PIQI_ERL_MOD)_piqi_rpc.erl


PIQI_ERLANG_FILES = $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


all: ebin


$(PIQI_ERLANG_FILES): $(PIQI_FILE)
	$(PIQIC) $(PIQIC_FLAGS) $<


clean::
	rm -f $(PIQI_ERLANG_FILES)


include $(PIQI_ROOT)/make/Makefile.erlang

