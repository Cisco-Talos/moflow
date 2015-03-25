American Fuzzy Lop + Dyninst == AFL Fuzzing blackbox binaries

The tool has two parts. The instrumentation tool and the instrumentation 
library. Instrumentation library has an initialization callback and basic 
block callback functions which are designed to emulate what AFL is doing
with afl-gcc/afl-g++/afl-as. 
Instrumentation tool (afl-dyninst) instruments the supplied binary by
inserting callbacks for each basic block and an initialization 
callback either at _init or at specified entry point.

Usage: ./afl-dyninst -i <binary> -o <binary> -l <library> -e <address> -s <number>
             -i: Input binary 
             -o: Output binary
             -l: Library to instrument (repeat for more than one)
             -e: Entry point address to patch (required for stripped binaries)
             -r: Runtime library to instrument (path to, repeat for more than one)
             -s: Number of basic blocks to skip
             -v: Verbose output

Switch -l is used to supply the names of the libraries that should 
be instrumented along the binary. Instrumented libraries will be copied
to the current working directory. This option can be repeated as many times
as needed. Depending on the environment, the LD_LIBRARY_PATH should be set 
to point to instrumented libraries while fuzzing. 

Switch -e is used to manualy specify the entry point where initialization
callback is to be inserted. For unstipped binaries, afl-dyninst defaults 
to using _init of the binary as an entry point. In case of stripped binaries
this option is required and is best set to the address of main which 
can easily be determined by disassembling the binary and looking for an 
argument to __libc_start_main. 

Switch -s instructs afl-dyninst to skip the first <number> of basic
blocks. Currently, it is used to work around a bug in Dyninst
but doubles as an optimization option, as skipping the basic blocks 
of the initialization rutines makes things run faster. If the instrumented
binary is crashing by itself, try skiping a number of blocks.

Switch -r allows you to specify a path to the library that is loaded
via dlopen() at runtime. Instrumented runtime libraries will be 
written to the same location with a ".ins" suffix as not to overwrite
the original ones. Make sure to backup the originals and then rename the
instrumented ones to original name. 

The instrumentation library "libDyninst.so" must be available in the current working
directory as that is where the instrumented binary will be looking for it.

Compiling:

1. Edit the Makefile and set DYNINST_ROOT and AFL_ROOT to appropriate paths. 
2. make

Example of running the tool:

Dyninst requires DYNINSTAPI_RT_LIB environment variable to point to the location
of libdyninstAPI_RT.so.

$ export DYNINSTAPI_RT_LIB=/usr/local/lib/libdyninstAPI_RT.so
$ ./afl-dyninst -i ./rar -o ./rar_ins -e 0x4034c0 -s 100
Skipping library: libAflDyninst.so
Instrumenting module: DEFAULT_MODULE
Inserting init callback.
Saving the instrumented binary to ./rar_ins...
All done! Happy fuzzing!

Here we are instrumenting  the rar binary with entrypoint at 0x4034c0
(manualy found address of main), skipping the first 100 basic blocks 
and outputing to rar_ins. 

Running AFL on instrumented binary

Since AFL checks if the binary has been instrumented by afl-gcc,AFL_SKIP_BIN_CHECK environment 
variable needs to be set. No modifications to AFL it self is needed. 
$ export AFL_SKIP_BIN_CHECK=1
Then, AFL can be run as usual:
$ afl-fuzz  -i testcases/archives/common/gzip/ -o test_gzip -- ./gzip_ins -d -c 



