#!/bin/bash

if [ $# -ne 1 ]
then
  echo "Usage: `basename $0` <cdep|tlv|recursive>"
  exit 1
fi
../../pin/pin -t ../../pintraces/obj-ia32/gentrace.so -taint_indices -taint_files input.txt -snapshot-file "sn" -- ./demo $1 input.txt
