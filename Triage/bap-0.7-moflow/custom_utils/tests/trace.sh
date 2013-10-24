#!/bin/bash
../../pin/pin -t ../../pintraces/obj-ia32/gentrace.so -taint_files input.txt -snapshot-file "sn" -- ./crash input.txt
