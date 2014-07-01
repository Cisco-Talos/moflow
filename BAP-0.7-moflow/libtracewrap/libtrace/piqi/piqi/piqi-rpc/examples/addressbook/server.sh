#!/bin/sh

PA="-pa $PIQI_ROOT/piqi-erlang/ebin -pa $PIQI_ROOT/piqi-rpc/ebin"

RUN='piqi_rpc:add_service({addressbook, addressbook_piqi_rpc, "addressbook"}).'

#erl $PA -s piqi_rpc start -eval "$RUN"
erl $PA -noshell -s piqi_rpc start -eval "$RUN"

