
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	$(PIQI_RPC_ERL_FILES) \
	addressbook.erl


ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
ERLC_FLAGS = #-pa $(PIQI_ROOT)/piqi-erlang #-I $(PIQI_ROOT)/piqi-erlang

EBIN_DIR = .


PIQIC = piqic erlang
PIQIC_RPC = piqic-erlang-rpc


PIQI_FILES = person.piqi
PIQI_RPC_FILES = addressbook.piqi


PIQI_ERL_FILES = \
	$(PIQI_FILES:%.piqi=%_piqi.erl)

PIQI_HRL_FILES = \
	$(PIQI_FILES:%.piqi=%_piqi.hrl)

PIQI_RPC_ERL_FILES = \
	$(PIQI_RPC_FILES:%.piqi=%_piqi.erl) \
	$(PIQI_RPC_FILES:%.piqi=%_piqi_rpc.erl) \
	$(PIQI_RPC_FILES:%.piqi=%_piqi_default_impl.erl)

PIQI_RPC_HRL_FILES = \
	$(PIQI_RPC_FILES:%.piqi=%_piqi.hrl) \
	$(PIQI_RPC_FILES:%.piqi=%_piqi_impl.hrl)


PIQI_ERLANG_FILES = $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)
PIQI_RPC_ERLANG_FILES = $(PIQI_RPC_ERL_FILES) $(PIQI_RPC_HRL_FILES)


all: ebin


$(PIQI_ERLANG_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i; \
	done


$(PIQI_RPC_ERLANG_FILES): $(PIQI_RPC_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC_RPC) $(PIQIC_FLAGS) $$i; \
	done


clean::
	rm -f $(PIQI_ERLANG_FILES) $(PIQI_RPC_ERLANG_FILES)
	rm -f addressbook person.piq.json


include $(PIQI_ROOT)/make/Makefile.erlang

