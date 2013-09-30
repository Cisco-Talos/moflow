
ERL_SOURCES = \
	piqirun_custom.erl \
	$(PIQI_ERL_FILES) \


ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
ERLC_FLAGS =
EBIN_DIR = .


PIQIC = piqic erlang
PIQIC_FLAGS = --gen-defaults


PIQI_FILES = example.piqi

PIQI_ERL_FILES = example_piqi.erl
PIQI_HRL_FILES = example_piqi.hrl


PRE_TARGET = $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


all: ebin


$(PIQI_ERL_FILES) $(PIQI_HRL_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i; \
	done


include $(PIQI_ROOT)/make/Makefile.erlang

