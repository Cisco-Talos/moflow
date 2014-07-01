/** Test if register n is in the context. */

#include "pin.H"
#include "pin_frame.h"
#include "pin_misc.h"

#include <iostream>
#include <sstream>
#include <cassert>
#include <cstdlib>

using namespace std;

KNOB<int> Num(KNOB_MODE_WRITEONCE, "pintool",
              "reg", "-1",
              "Reg number");

void InstrTrace(TRACE trace, VOID *v) {
  exit(0);
}

int main(int argc, char *argv[]) {

  PIN_Init(argc, argv);

  cout << Num << endl;

  if (Num == -1) {
    cout << "You must define a reg" << endl;
    return 1;
  }

  CONTEXT fakectx;

  PIN_GetContextReg(&fakectx, static_cast<REG> (Num.Value()));

  assert (GetTypeOfReg(static_cast<REG> (Num.Value())) != VT_NONE);
  
  TRACE_AddInstrumentFunction(InstrTrace, 0);

  exit(0);

}
