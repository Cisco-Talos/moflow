AM_CXXFLAGS = -I./pb -I@LIBTRACE_DIR@/src/common -I@LIBTRACE_DIR@/protobuf/src $(CXXFLAGS)

lib_LIBRARIES = libtrace.a
libtrace_a_SOURCES = pb/frame.piqi.pb.h pb/frame.piqi.pb.cc trace.container.cpp

# Don't auto-link LIBS coming from autoconf, because we might not have
# 32-bit equivalents!
LIBS =

SUBDIRS = . utils