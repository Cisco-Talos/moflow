#!/bin/bash
./autogen.sh &&
# compile with z3 bindings
sudo apt-get install libgmp3c2 -y --force-yes &&
cd solvers &&
./getz3.sh &&
cd .. &&
./configure --with-z3=`pwd`/solvers/z3 &&
(cd ocaml; make clean) &&
make &&
cd pintraces &&
./getpin.sh &&
make &&
cd ../custom_utils &&
cd egas &&
make &&
cd .. &&
make all &&
cd tests &&
make &&
cd ../.. &&
if ! which stp > /dev/null; then
  echo -e "STP not found, installing..."
  svn co https://svn.code.sf.net/p/stp-fast-prover/code/trunk/stp stp &&
  cd stp &&
  sudo ./clean-install.sh &&
  cd .. 
fi


