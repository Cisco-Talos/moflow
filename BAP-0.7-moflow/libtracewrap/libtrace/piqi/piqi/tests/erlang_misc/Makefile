
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \


ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
ERLC_FLAGS =
EBIN_DIR = .


PIQIC = piqic erlang
PIQIC_FLAGS = --gen-defaults


PIQI_FILES = empty-record.piqi


PIQI_ERL_FILES = empty_record_piqi.erl
PIQI_HRL_FILES = empty_record_piqi.hrl

PIQI_ERLANG_FILES = $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


PRE_TARGET = $(PIQI_ERLANG_FILES)


all: ebin


$(PIQI_ERLANG_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i; \
	done


include $(PIQI_ROOT)/make/Makefile.erlang

