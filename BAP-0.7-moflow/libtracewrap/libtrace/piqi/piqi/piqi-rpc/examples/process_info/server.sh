#!/bin/sh

PA="-pa $PIQI_ROOT/piqi-erlang/ebin -pa $PIQI_ROOT/piqi-rpc/ebin"

RUN='piqi_rpc:add_service({process_info_example, process_info_piqi_rpc, "process-info"}).'

erl $PA -noshell -s piqi_rpc start -eval "$RUN"

