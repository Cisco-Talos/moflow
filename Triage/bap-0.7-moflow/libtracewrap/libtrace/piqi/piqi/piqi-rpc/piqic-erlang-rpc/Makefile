include ../../Makefile.config

PIQIC_PLUGIN = piqic-erlang-rpc


PIQIC_PLUGIN_BEAMS = \
    $(PIQI_ROOT)/piqi-erlang/ebin/piqi.beam \
    $(PIQI_ROOT)/piqi-erlang/ebin/piqirun.beam \
    $(PIQI_ROOT)/piqi-erlang/ebin/piqi_piqi.beam \
    $(PIQI_ROOT)/piqi-erlang/ebin/piqic_erlang_ext.beam \
    ../ebin/piqic_erlang_rpc.beam


all: $(PIQIC_PLUGIN)


$(PIQIC_PLUGIN): $(PIQIC_PLUGIN_BEAMS)
	$(PIQI_ROOT)/make/make_escript $@ $(PIQIC_PLUGIN_BEAMS)
	chmod +x $@


clean::
	rm -f $(PIQIC_PLUGIN)

