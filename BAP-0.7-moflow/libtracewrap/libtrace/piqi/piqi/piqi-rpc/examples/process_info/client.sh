#!/bin/sh

set -x # -e


piqi call http://localhost:8888/process-info -p


piqi call http://localhost:8888/process-info -h


piqi call -t json http://localhost:8888/process-info/list-processes
curl -v -X POST -H 'Accept: application/json' 'http://localhost:8888/process-info/list-processes'


piqi call -t json http://localhost:8888/process-info/process-info -- "<0.12.0>"

curl -v -X POST -H 'Accept: application/json' -H 'Content-Type: application/json' --data-binary '{"pid" : "<0.12.0>"}' 'http://localhost:8888/process-info/process-info'


piqi call -t json http://localhost:8888/process-info/list-process-info -- -a -d -m

curl -v -X POST -H 'Accept: application/json' -H 'Content-Type: application/json' --data-binary '["all", "dictionary", "messages"]' 'http://localhost:8888/process-info/list-process-info'

