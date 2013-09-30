slicer
------

Compile BAP, then just:
make slicer

Options:
% ./slicer -help 
Usage: ./slicer <options>

  -il <trace.il>
  -var <var to slice by>
  -f slice forward
  -b slice backward (default)
  -o output file (default: /dev/stdout)
  -help  Display this list of options
  --help  Display this list of options

Trace has to be concretized with -trace-concrete-subst. This option turns every
memory deref into a variable, so instead of mem[addr] (variable name + index),
we have: mem_addr. To get unique variable names, you can use the "-trace-dsa"
option in iltrans. Unique names make it easier to slice by variables set in
the middle of the trace -- there's no need to cut traces manually. prep-slice.sh 
does both "-trace-concrete-subst" and "-trace-dsa".


Example
-------

./prep-slice.sh trace.bpt trace.il
./slicer -il trace.il -var symb_1 -f

will result in a forward slice of variable symb_1. 

Only data dependencies are captured exactly. Asserts (conditional jumps) are
included iff their expressions contain variables influenced by (in case of 
forward slices), or influencing (backward slices) the slicing source/sink (-var
argument).

***

Beware of pretty printing serialized traces to .il without any kind of 
processing:

iltrans -serializedtrace trace.bpt -pp-ast nice.il
iltrans -il nice.il -trace-concrete -pp-ast concrete.il

will not give the same result as:

iltrans -serializedtrace trace.bpt -trace-concrete -pp-ast concrete.il

This happens because contents of memories are not serialized/deserialized 
correctly.


motriage
--------

This tool is supposed to explore possible execution futures from the crash point
and identify paths which lead to tainted writes or tainted jumps. 

Example invocation:
./motriage -sn tests/sn -dot g.dot

-sn passes a memory snapshot, collected with gentrace.so
-dot sets the filename for the output graph in graphviz dot format
-jd sets the max. number of analyzed branches (default: 5)
-id sets the max. number of analyzed instructions (default: 16)
-symb_taint makes all tainted variables symbolic

  Here's an example of how to capture a snapshot of a synthetic example 
  (tests/test.c, compiled with 'make'):

  pin -t ../../pintraces/obj-ia32/gentrace.so -taint_files input.txt 
  -snapshot-file "sn" -- ./crash input.txt

  tests/sn is the resulting snapshot file (big, because we capture everything,
  included system modules)

The output dot file needs to be "postprocessed", because graphviz library for
ocaml doesn't support html attributes:

cat g.dot | sed "s/\"</<</g" | sed "s/>\"/>>/g"  > h.dot

Now we can render it to postscript:

dot -Tps h.dot -o h.ps && gv h.ps

All of the postprocessing steps are captured in triage.sh script. Invoke it like
so:

./triage.sh <snapshot file> <output .ps file>

Paths in graph can with an error (red ellipse) or success (green ellipse).  
Success means that we encountered a tainted write or a tainted jump. In both
cases we stop, since such condition is likely to be exploitable.

Possible errors:
- reached limit of branches or instructions,
- untainted symbolic write (we don't know what was overwritten),
- jump to unknown (and untained) location,
- unsatisfiable path. Example: mov eax, 1 / cmp eax, 1 / jnz foo -- jump won't
  be taken, because eax=1. We detect such (and other) cases by quering a solver
  if a certain path predicate is satisfiable.

Limitations:
- path explosion: for n branches, there're 2^n paths, so setting jd=32 would
  force motriage to explore 2^32 possible paths. High constants are guaranteed
  to choke it.
- reads from unmapped memory are implemented by creating a new symbolic variable
  which can take any value (is unconstrained). This assumption is unsound and
  makes reported successes possibly incorrect -- during 'real' execution, these
  values can be constrained in ways which make it impossible to take a certain
  path.

