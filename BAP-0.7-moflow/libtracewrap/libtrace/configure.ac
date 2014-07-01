#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.64])
AC_INIT([libtrace], [1.0], [bap-dev@lists.andrew.cmu.edu])
AC_CONFIG_SRCDIR([src/cpp/trace.container.hpp])
AC_CONFIG_HEADERS([src/cpp/config.h])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE

AC_SUBST(LIBTRACE_DIR)
LIBTRACE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

AC_SUBST(INCDIRS)

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_RANLIB
AC_PROG_OCAML

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lpthread':
AC_CHECK_LIB([pthread], [main])

AC_ARG_WITH([batteries],
            [AS_HELP_STRING([--with-batteries],
            [specify path to batteries])],
            [with_batteries="$( cd $withval && pwd )"],
            [])

AS_IF([test "x$with_batteries" != x],
      [AC_CHECK_OCAML_MODULE(has_batteries,batteries,[BatHashtbl],[$with_batteries])
       AS_IF([test "x$has_batteries" = "xno"],
             [AC_MSG_FAILURE([batteries not found in $with_batteries])],
             [AC_SUBST(BAT_DIR, [$with_batteries])])],
      [AC_MSG_FAILURE([You must specify the path to batteries using --with-batteries])])

# Checks for header files.
AC_CHECK_HEADERS([stdint.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_SIZE_T
AC_TYPE_UINT64_T
AC_SYS_LARGEFILE
AC_FUNC_FSEEKO

# Checks for library functions.
AC_CHECK_FUNCS([memset])

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 src/cpp/Makefile
                 src/cpp/utils/Makefile
                 src/ocaml/Makefile
                 src/ocaml/utils/Makefile])
AC_OUTPUT

# Configure google protobuf
# Note: protobuf must be compiled for 32-bit because it is linked by pintraces
# code, which is 32-bit
(cd protobuf && CFLAGS=-m32 CXXFLAGS=-m32 ./configure)
