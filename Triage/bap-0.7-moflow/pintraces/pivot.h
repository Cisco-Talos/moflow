#ifndef __PIVOT_H
#define __PIVOT_H
/**
   @author Edward J. Schwartz

   $Id: pivot.h 6599 2012-08-12 19:07:47Z edmcman $
*/

#include "pin.H"
#include "pin_frame.h"
#include "pin_taint.h"
#include <set>
#include <stdint.h>

/** 
    Types of pivots.  

    Switchstack is ESP <- reg + c.
    Mempivot is ESP <- M[reg + c]. 
*/
typedef enum { UNKNOWN, SWITCHSTACK, MEMPIVOT } pivottype_t;

/** Pivot objects */
bool operator<(const struct pivot_s &a, const struct pivot_s &b);

typedef struct pivot_s {
  uint64_t address; /** Gadget address. */
  pivottype_t t; /** Type of pivot. */
  REG base; /** Base register. */
  uint64_t offset; /** Offset */
} pivot_t;

/** Sets of pivot objects */
typedef set<pivot_t> pivot_set;

/** Function headers */
pivot_set PIVOT_parseinput(istream &f);
void PIVOT_testpivot(pivot_set, CONTEXT *, pintrace::TaintTracker &);
ostream& operator<<(ostream &o, const pivot_s &);

#endif
