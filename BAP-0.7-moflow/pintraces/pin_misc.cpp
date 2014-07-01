/** Given a REG, return a trace type (or VT_NONE for failure) */

#include "pin.H"
#include "pin_frame.h"

uint32_t GetTypeOfReg(REG r) {
  if (REG_is_gr8(r)) return VT_REG8;
  if (REG_is_gr16(r)) return VT_REG16;
  if (REG_is_gr32(r)) return VT_REG32;
  if (REG_is_gr64(r)) return VT_REG64;

  string s = REG_StringShort(r);

  if (s == "eip" || s == "eflags") {
    // No problem for these
    return VT_REG32;
  }

  // Otherwise, print a warning...
  
  cerr << "Warning: Unknown register size of register " << REG_StringShort(r) << endl;
  return VT_NONE;
}
