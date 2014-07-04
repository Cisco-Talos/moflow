#!/usr/bin/env bash

# $Id: getpin.sh 7415 2013-04-25 18:03:11Z edmcman $
# Download and extract Pin

set -x

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $DIR

# check if pin dir exists first

wget 'https://software.intel.com/sites/landingpage/pintool/downloads/pin-2.12-58423-gcc.4.4.7-linux.tar.gz' -U "Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0" -O - | tar -xvz -C ..
rm -rf ../pin
mv ../pin-* ../pin
#make
