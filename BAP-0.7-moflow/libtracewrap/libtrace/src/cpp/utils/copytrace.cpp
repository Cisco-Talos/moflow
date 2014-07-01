/** $Id$
 *
 * Copy one trace to another to test API.
 */

#include <fstream>
#include <iostream>
#include "trace.container.hpp"

using namespace SerializedTrace;

void copy_all(TraceContainerReader &r, TraceContainerWriter &w) {
  while (!r.end_of_trace()) {
    w.add(*(r.get_frame()));
  }
}

int main(int argc, char **argv) {
  if (argc != 3) {
    if (argv[0]) {
      std::cout << "Usage: " << argv[0] << " <source filename> <destination filename>" << std::endl;
    }
    exit(1);
  }
  std::string srcfile(argv[1]);
  std::string dstfile(argv[2]);

  TraceContainerReader r(srcfile);
  TraceContainerWriter w(dstfile, r.get_arch(), r.get_machine(), r.get_frames_per_toc_entry(), true);

  copy_all(r, w);
}
