include ../../Makefile.config

PIQIC_PLUGIN = piqic-erlang-ext


PIQIC_PLUGIN_BEAMS = \
    ../ebin/piqi.beam \
    ../ebin/piqirun.beam \
    ../ebin/piqi_piqi.beam \
    ../ebin/piqic_erlang_ext.beam


all: $(PIQIC_PLUGIN)


$(PIQIC_PLUGIN): $(PIQIC_PLUGIN_BEAMS)
	$(PIQI_ROOT)/make/make_escript $@ $(PIQIC_PLUGIN_BEAMS)
	chmod +x $@


clean::
	rm -f $(PIQIC_PLUGIN)

