#!/bin/bash

if [ $# -ne 1 ]
then
  echo "Usage: `basename $0` <serializedtrace.bpt>"
  exit 1
fi
../utils/iltrans -serializedtrace $1 -pp-ast /dev/stdout

