#!/bin/bash -e
./autogen.sh
# compile with z3 bindings
sudo apt-get install libgmp10 libgmp-dev libiberty-dev nasm exuberant-ctags \
 -y --force-yes
# The link the so that libz3 will expect
#sudo ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10.2.0 /usr/lib/x86_64-linux-gnu/libgmp.so.3
cd solvers
./getz3.sh
cd ..
# If ocamlgraph is already installed, these will fail. If it's not installed, 
# you need them
#cd ocamlgraph-1.8
#./configure
#make
#make install-findlib
#cd ..
./configure --with-z3=`pwd`/solvers/z3
(cd ocaml; make clean)
make
cd pintraces
./getpin.sh
make
cd ../custom_utils
cd egas
make
cd ..
make all
cd tests
make
cd ..
