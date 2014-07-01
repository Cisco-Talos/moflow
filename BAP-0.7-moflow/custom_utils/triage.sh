#!/bin/bash

if [ $# -ne 2 ]
then
  echo "Usage: `basename $0` <snapshot filename> <.ps filename>"
  exit 1
fi
./motriage -id 256 -jd 16 -sn $1 -dot g.dot && cat g.dot | sed "s/\"</<</g" | sed "s/>\"/>>/g"  > h.dot
rm g.dot
dot -Tps h.dot -o $2 && rm h.dot && gv $2

