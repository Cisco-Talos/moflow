/** Automatically generate the list of PIN registers accessible
 * through the context interface. We only list registers whose types
 * we know.
 *
 * @author ejs
 */

#include "pin.H"

#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cassert>

int main(int argc, char *argv[]) {

  if (argc != 2) {
    std::cout << "Expected one filename argument" << std::endl;
    exit(1);
  }

  size_t count = 0;
  std::ofstream out(argv[1]);
  assert (out.good());
  out << "/* Null terminated list of PIN register ids that can be accessed from the context */" << std::endl;
  out << "int32_t pinctxregs[] = { ";
  
  for (REG r = static_cast<REG> (0); r <= LEVEL_BASE::REG_LAST; r = static_cast<REG> (r+1)) {
    //std::cerr << "Trying " << r << std::endl;

    std::stringstream ss;
    ss << r;
    //std::cout << ss.str() << std::endl;

#ifdef _WIN32
	string cmd("..\\pin\\pin.bat -t .\\obj-ia32\\ctxtest.dll -reg " + ss.str() + " -- .\\listregs.exe");
#else
    string cmd("../pin/pin -t ./obj-ia32/ctxtest.so -reg " + ss.str() + " -- ./listregs");
#endif
    //std::cout << "cmd: " << cmd << std::endl;
    
    if (0 == system(cmd.c_str())) {
      //std::cerr << "success" << std::endl;
      count++;
      out << r << ", ";
    }
  } // end of for loop
  out << "-1 }; ";

  out << endl << endl;

  out << "/* Number of meaningful entries in pinctxregs */" << endl;
  out << "size_t pinctxregs_size = " << count << ";" << endl;
  
  return 0;
}
