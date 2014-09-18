# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.


#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Bap, 0.7, bap-dev@lists.andrew.cmu.edu)
AC_CONFIG_SRCDIR([libasmir/src/asm_program.c])
AC_CONFIG_HEADER([libasmir/config.h])

AM_INIT_AUTOMAKE

m4_include([./m4/ac_ocaml.m4])
m4_include([./m4/ac_prog_latex.m4])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_PROG_OCAML([3.12.0])
AC_PROG_CAMLP4
AC_PROG_OCAMLFIND
AC_PROG_CAMLIDL
#AC_PROG_LIBTOOL


# Check for Z3
AC_ARG_WITH([z3],
            [AS_HELP_STRING([--with-z3],
              [location of Z3 library])],
            [with_z3=$withval],
            [with_z3=no])

# Save pre-z3 ldflags
OLDLDFLAGS=$LDFLAGS

AS_IF([test "x$with_z3" != "xno"],
     [
     LDFLAGS="$OLDLDFLAGS -L$with_z3/lib"
     AC_CHECK_LIB([z3], [Z3_mk_bvadd],
       [
        z3_dir=$with_z3
        with_z3=yes
        AC_DEFINE([HAVE_Z3_BINDINGS], [1], [Define to use Z3 bindings])
        ],

       [AC_MSG_FAILURE([z3 library not found])],
       [-lgmp -lgomp -lrt]
       )])

# Restore
LDFLAGS=$OLDLDFLAGS

rm -f solvers/makefile.inc
if `test "x$with_z3" = "xyes"`; then
   cat >>solvers/makefile.inc <<"EOF"
SOLVER_PPFLAGS=-D WITH_Z3_BINDINGS
SOLVER_CLIBS = z3stubs z3
EOF
echo "SOLVER_SOURCES = $z3_dir/ocaml/z3.ml" >>solvers/makefile.inc
echo "SOLVER_LIBDIRS = $z3_dir/lib" >>solvers/makefile.inc
echo "SOLVER_LDFLAGS = -fopenmp -Wl,-rpath=$z3_dir/lib" >> solvers/makefile.inc
else
   touch solvers/makefile.inc
fi

# Add -DAMD64 when needed
if test "$(uname -m)" == "x86_64";
   then export CFLAGS="$CFLAGS -DAMD64";
fi

# Check for PIN
PIN_HOME=${PIN_HOME:=$(pwd)/pin}
AC_CHECK_FILE([$PIN_HOME],
              [HAVE_PIN=true],
              [HAVE_PIN=false; AC_MSG_WARN([Pin not found. BAP pintraces will be unavailable.])])

AM_CONDITIONAL([HAVE_PIN], [test x$HAVE_PIN = xtrue])

# Conditionally build llvm
AC_MSG_CHECKING([llvm ocaml libraries])
AC_ARG_WITH([llvm],
            [AS_HELP_STRING([[--with-llvm=[=ARG]]], [use OCaml llvm package named ARG])],
            LIBLLVM="$withval",
            LIBLLVM="check")

AS_IF([test x$LIBLLVM = xno],
      [LLVM_PACKAGE=""],
      [AS_IF([test x$LIBLLVM = xyes -o x$LIBLLVM = xcheck],
             [AS_IF([ocamlfind query llvm >/dev/null 2>&1],
             [LLVM_PACKAGE="llvm"],
             [AS_IF([ocamlfind query llvm-3_1 >/dev/null 2>&1],
                    [LLVM_PACKAGE="llvm-3_1"],
                    [LLVM_PACKAGE=""])])],
             [AS_IF([ocamlfind query $LIBLLVM >/dev/null 2>&1],
                    [LLVM_PACKAGE="$LIBLLVM"],
                    [])])])

AS_IF([test \( x$LIBLLVM != xno -a x$LIBLLVM != xcheck \) -a x$LLVM_PACKAGE = x],
      [AC_MSG_FAILURE([--with-llvm specified but llvm was not found])],
      [])

AC_SUBST(LLVM_PACKAGE)

AS_IF([test x$LLVM_PACKAGE != x], [HAVE_LLVM_AC=yes], [HAVE_LLVM_AC=no])
AS_IF([test x$HAVE_LLVM_AC = xyes],
      [AC_MSG_RESULT([$HAVE_LLVM_AC ($LLVM_PACKAGE)])],
      [AC_MSG_RESULT([$HAVE_LLVM_AC])])
AC_SUBST(HAVE_LLVM_AC)


AS_IF([test "x$HAVE_LLVM_AC" = "xtrue"],
      [LLVM_PPFLAGS="-D WITH_LLVM"],
      [])
AC_SUBST(LLVM_PPFLAGS)

# Check for etags
AS_IF([which etags >/dev/null 2>&1],
      [HAVE_ETAGS=true],
      [HAVE_ETAGS=false; AC_MSG_WARN([ETAGS not found. BAP will not build tags.])])

AM_CONDITIONAL([HAVE_ETAGS], [test x$HAVE_ETAGS = xtrue])

# Checks for ocaml libraries
AC_MSG_CHECKING([for camomile])
ocamlfind query camomile >/dev/null 2>&1
if test $? != 0; then
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([camomile ocaml package not found])
fi
AC_MSG_RESULT([yes])

VEX_DIR=`pwd`/'VEX'
VEX_VERSION=2201

# Checks for libraries.
AC_CHECK_LIB([bfd], [bfd_init], , AC_MSG_ERROR([BFD library not found]))
AC_CHECK_LIB([iberty], [xmalloc], , AC_MSG_ERROR([Iberty library not found]))
AC_CHECK_LIB([pcre], [pcre_config], , AC_MSG_ERROR([PCRE library not found]))
# zlib is apparently needed by bfd
AC_CHECK_LIB([z], [inflate], , AC_MSG_ERROR([zlib library not found]))

AC_SUBST(VEX_DIR)

AC_SUBST(INCDIRS)

rm -f libasmir/makefile.inc
echo "VEX_DIR=${VEX_DIR}" >> libasmir/makefile.inc
echo "LIBASMIR_CXXFLAGS=-I${VEX_DIR}/pub -I`pwd`/src/include $CXXFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LDFLAGS=-L${VEX_DIR} -L`pwd`/src $LDFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LIBS=-lasmir -lvex -lbfd -ldl -liberty -lz" >> libasmir/makefile.inc
echo "CLIBS=asmir vex opcodes bfd z iberty stdc++" >> libasmir/makefile.inc
AC_SUBST(ASMIR_DIR)
ASMIR_DIR="`pwd`/libasmir"

AC_SUBST(BAP_DIR)
BAP_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdint.h stdlib.h string.h unistd.h], ,
   AC_MSG_ERROR([Standard headers missing]))
AC_CHECK_HEADER([bfd.h], ,
   AC_MSG_ERROR([You need BFD headers (e.g., libbfd-dev)]))
AC_CHECK_HEADER([pcre.h], ,
   AC_MSG_ERROR([You need libpcre headers (e.g., libpcre3-dev)]))

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_SIZE_T
AC_DEFINE_UNQUOTED([VEX_VERSION], [${VEX_VERSION}], [The VEX SVN Version])

AC_CHECK_SIZEOF(bfd_vma, [], [#include <bfd.h>])

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_CHECK_FUNCS([bzero memset strstr strtoul])


# Check for library function versions

AC_CONFIG_FILES([Makefile
                 libasmir/Makefile
                 libasmir/apps/Makefile
                 libasmir/src/Makefile
                 libtracewrap/Makefile
		 ocaml/Makefile
                 utils/Makefile
                 tests/Makefile])
AC_OUTPUT

# Configure ocamlgraph
(cd ocamlgraph-1.8 && ./configure)

# Configure zarith
(cd zarith-1.0 && ./configure)

# Force libtracewrap to reconfigure
rm -f libtracewrap/CONFIGURED
