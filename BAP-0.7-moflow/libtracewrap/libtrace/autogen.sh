aclocal -I m4
autoconf
autoheader
automake --add-missing --copy
(cd protobuf && ./autogen.sh)