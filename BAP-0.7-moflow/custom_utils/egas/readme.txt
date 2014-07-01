EGAS
----

EGAS is a SAGE [0] clone. Given a sample input file, it's supposed to generate
new files which exercise different code paths in the target executable.

Usage
-----

Simple invocation:

./egas -app test/bof1 -seed test/input.txt

-app - path to the target app
-seed - path to the seed file

New samples and various metadata files are going to be stored in directory set 
with the -samples-dir parameter (default: ./samples). Crashes are going to be 
stored in directory set with the -crashes-dir parameter (default: ./crashes).

Types of files stored in samples dir:

<id>.sol - input file that exercises a particular code path
<id>.bb - list of newline separated hex numbers (in ASCII format, like 0x112233) 
  representing addresses of basic blocks visited tracing of the target
  app with input file set to <id>.sol
<id>.bpt - serialized trace of app with input file set to <id>.sol
<id>.conc.il - concretized version of <id>.bpt. To produce this file with 
  iltrans, invoke: 
  iltrans -serializedtrace <id>.bpt -trace-concrete-subst -trace-dce -pp-ast
  <id>.conc.il
  Explanation:
  -trace-concrete-subst - performs concretization, but replaced each memory
    dereference with a variable, so instead of var := load(memory, idx) we get
    var := mem_var_idx. This is useful for performing some optimizations during
    solver interaction later on.
  -trace-dce - deadcode elimination.
  -pp-ast - pretty printing to a file.
<id>.exn - if the target app crashes during coverage (basic block) tracing, the
  exception number and address are written to this file. For example:
  Exception 11 at 0x5a5e3155.

Files stored in crashes dir:

yyyy-mm-dd_hh:mm:ss_id.crash - <id>.sol file from samples dir, renamed to 
  contain date/time info. For example: 2013-12-29_08:15:46_1953.crash

Here's a more complicated invocation of egas:

./egas -app test_app -seed seed.file -fmt "-opt1 val -opt2 val2 -input %s" 
       -samples-dir custom_dir1 -crashes-dir custom_dir2

-fmt "ARGS" - command line passed to test_app. If ARGS contains the "%s" marker, 
  then it's going to be replaced with -seed parameter value. In the above 
  example, %s is going to be replaced with "seed.file".

Additional command line options (useful for debugging):
-flip-limit n - sets the max. number of flips of a branch to n. Flipping a 
  branch means trying to find an input that forces executing down a path with
  a negated branch. We want to limit the number of flips of the same branch.
  This situation is encountered in loops.
-flip-one n - for debugging purposes. Flip just branch n and then exit.


0 - http://research.microsoft.com/en-us/um/people/pg/
