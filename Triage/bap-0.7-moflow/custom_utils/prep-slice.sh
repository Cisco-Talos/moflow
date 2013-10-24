#!/bin/bash

#-concrete-subst changes memory derefs into variables
#-trace-dsa makes all variable names unique, so it's not necessary to cut
#traces by hand before passing to slicer

if [ $# -ne 2 ]
then
  echo "Usage: `basename $0` <serialized.trace.bpt> <output.il>"
  exit 1
fi
../utils/iltrans -serializedtrace $1 -trace-concrete-subst -trace-dsa -pp-ast $2
