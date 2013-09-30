#!/bin/bash
./autogen.sh &&
./configure &&
make &&
cd pintraces &&
./getpin.sh &&
make &&
cd ../custom_utils &&
make all &&
cd tests &&
make &&
cd ../.. &&
if ! which stp > /dev/null; then
  echo -e "STP not found, installing..."
  svn co https://stp-fast-prover.svn.sourceforge.net/svnroot/stp-fast-prover/trunk/stp stp
  cd stp
  sudo ./clean_install.sh   
  cd ..
fi

