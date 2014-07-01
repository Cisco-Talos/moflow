#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if test "$(uname -m)" == "x86_64"; then
    url="http://research.microsoft.com/projects/z3/z3-x64-3.2.tar.gz"
else
    url="http://research.microsoft.com/projects/z3/z3-3.2.tar.gz"
fi

wget $url -O - | tar -xvz -C $DIR
cd $DIR/z3/ocaml && ./build-lib.sh $(ocamlfind query unix)