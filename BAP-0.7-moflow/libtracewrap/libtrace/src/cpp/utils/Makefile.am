AM_CXXFLAGS = -I../pb -I.. -I@LIBTRACE_DIR@/src/common -I@LIBTRACE_DIR@/protobuf/src
AM_CXXFLAGS += -m32

utils_LDADD = ../libtrace.a @LIBTRACE_DIR@/protobuf/src/.libs/libprotobuf.a -lpthread

bin_PROGRAMS = readtrace copytrace
readtrace_SOURCES = readtrace.cpp
readtrace_LDADD = $(utils_LDADD)
copytrace_SOURCES = copytrace.cpp
copytrace_LDADD = $(utils_LDADD)

# Don't auto-link LIBS coming from autoconf, because we might not have
# 32-bit equivalents!
LIBS =