DYNINST_ROOT = /usr
AFL_ROOT = /path/to/afl/root/
DYNINST_INCLUDE = $(DYNINST_ROOT)/include
DYNINST_LIB =  $(DYNINST_ROOT)/lib
# These should point to where libelf and libdwarf are installed
LOCAL_INC_DIR = /usr/local/include
LOCAL_LIBS_DIR = /usr/local/lib

CXX = g++
CXXFLAGS = -g -Wall -O3
LIBFLAGS = -fpic -shared

CC = gcc
CFLAGS = -Wall -pedantic -g -std=gnu99


all: afl-dyninst libAflDyninst.so

afl-dyninst: afl-dyninst.o
	$(CXX) $(CXXFLAGS) -L$(DYNINST_LIB) \
		-L$(LOCAL_LIBS_DIR) \
		-o afl-dyninst afl-dyninst.o \
		-lcommon \
		-liberty \
		-ldyninstAPI 

libAflDyninst.so: libAflDyninst.cpp
	$(CXX) $(CXXFLAGS) $(LIBFLAGS) -I$(AFL_ROOT) libAflDyninst.cpp -o libAflDyninst.so

afl-dyninst.o: afl-dyninst.cpp
	$(CXX) $(CXXFLAGS) -I$(LOCAL_INC_DIR) -I$(DYNINST_INCLUDE)  -c afl-dyninst.cpp

clean:
	rm -f afl-dyninst *.so *.o 
