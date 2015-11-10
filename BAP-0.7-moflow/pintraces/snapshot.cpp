#include "pin.H"

#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stack>
#include <vector>
#include <cstring>
#include <stdint.h>
#include <cstdlib>
#include <time.h>

#include "pin_frame.h"
#include "pin_trace.h"
#include "reg_mapping_pin.h"
#include "cache.h"

/* The new trace container format */
#include "trace.container.hpp"

#include "pivot.h"

#include "pin_taint.h"
#include "snapshot.h"

using namespace pintrace;
using namespace SerializedTrace;

//////////////////////////////////////////////////////////////
// Take a snapshot of process memory. 
//////////////////////////////////////////////////////////////
size_t ReadMem(uint8_t *dst, uint32_t src, size_t size){
  EXCEPTION_INFO exn;
  EXCEPTION_CLASS exn_class;
  EXCEPTION_CODE exn_code;
  size_t read_size;

  read_size = PIN_SafeCopyEx((void*)dst, (const void*)src, size, &exn);
  if(read_size != size){
    exn_code = PIN_GetExceptionCode(&exn);
    exn_class = PIN_GetExceptionClass(exn_code);
    if(exn_class != EXCEPTCLASS_ACCESS_FAULT){
      cerr << "ERROR: PIN_SafeCopyEx returned an unexpected exception \
        code" << endl;
      cerr << PIN_ExceptionToString(&exn) << endl;
      assert(false);
    }
  }
  return read_size;
}


// don't capture too much stack memory
void ClampStack(u32 esp, u32 lo, u32 hi, u32 *new_lo, u32 *new_hi){
  assert(lo <= esp && esp <= hi);
  assert(esp >= MAX_STACK); 

  *new_lo = MAX(lo, esp-(MAX_STACK));
  //*new_hi = MIN(hi, esp+(MAX_STACK/2));
}

// return the number of captured regions
u32 CollectRegions(const CONTEXT *ctx, region_t regions[]){
  uint8_t *dst;
  uint32_t i, src, lo, hi, rcount;
  size_t size, total_size;
  ADDRINT esp;

  rcount = 0;
  esp = PIN_GetContextReg(ctx, REG_STACK_PTR);

  dst = (uint8_t*)malloc(PAGE_SIZE);
  if(dst == NULL){
    cerr << "ERROR: CollectRegions, malloc failed" << endl;
    assert(false);
    return 1;
  }
  total_size = src = lo = hi = 0;
  for(i=0;i<N_PAGES;i++){
    src += PAGE_SIZE;
    size = ReadMem(dst, src, PAGE_SIZE);
    if(size != PAGE_SIZE){
      if(lo > 0){
        hi = src;
        if(lo <= esp && esp <= hi){
          ClampStack(esp, lo, hi, &lo, &hi);
        }
        assert(lo < hi);
        if(rcount == MAX_REGIONS){
          free(dst);
          return 0;
        }
        regions[rcount].lo = lo;
        regions[rcount].hi = hi;
        regions[rcount].size = hi-lo;
        rcount++;
        fprintf(stderr, "0x%08x - 0x%08x (%uKB)\n", lo, hi, (hi-lo)/1024);
        total_size += (hi-lo);
        lo = hi = 0;
      }
    }
    else{
      if(lo == 0){
        lo = src;
      }
    }
  }
  fprintf(stderr, "Total snapshot size: %uB (%dKB) (%dMB)\n", 
      total_size, total_size/1024, total_size/(1024*1024));

  //We have to leak this, since freeing 'dst' would invalidate the regions 
  //table. 
  //free(dst);

  return rcount;
}

void write_u32(FILE *f, u32 v){
  size_t sz;

  assert(sizeof(u32) == 4);
  sz = fwrite(&v, 1, sizeof(u32), f);
  assert(sz == sizeof(u32));
}

void EmitHeader(FILE *f){
  write_u32(f, TAG_START);
  write_u32(f, FORMAT_VERSION);
}

void EmitRegionTable(FILE *f, region_t regions[], u32 rcount){
  u32 i;
  region_t *r; 

  write_u32(f, TAG_REGIONS);
  write_u32(f, rcount);
  for (i = 0; i < rcount; i++) {
    r = &regions[i];
    write_u32(f, r->lo);
    write_u32(f, r->size);
  }
}

void EmitTaintMap(FILE *f, map<u32,u32> memory){
  map<u32,u32>::iterator it, ie;

  write_u32(f, TAG_TAINT);
  write_u32(f, memory.size());
  for (it = memory.begin(), ie = memory.end() ; it != ie ; ++it){
    write_u32(f, it->first); 
    write_u32(f, it->second);
  }
}

//[reg id, reg value, taint]* [REG_INVALID_, 0, 0]
void EmitRegisters(FILE *f, const CONTEXT *ctx, context &delta){
  ADDRINT v, taint;
  int i, sz;
  //XXX: if you change order of these ids, you also need to change numbering
  //in motriage.ml, function: rid2reg
  REG regs[] = {LEVEL_BASE::REG_EAX, LEVEL_BASE::REG_EBX, LEVEL_BASE::REG_ECX,
              LEVEL_BASE::REG_EDX, LEVEL_BASE::REG_ESI, LEVEL_BASE::REG_EDI, LEVEL_BASE::REG_EBP,
              LEVEL_BASE::REG_ESP, LEVEL_BASE::REG_EFLAGS, LEVEL_BASE::REG_EIP};

  context::iterator it;

  sz = sizeof(regs)/sizeof(regs[0]);

  write_u32(f, TAG_REGS);
  write_u32(f, sz);

  for(i=0;i<sz;i++){
    REG r = regs[i];
    v = PIN_GetContextReg(ctx, r);
    write_u32(f, i); //we don't use PIN's ids
    write_u32(f, v);
    it = delta.find(r);
    if(it != delta.end()){
      taint = it->second;
    }
    else{
      taint = 0;
    }
    write_u32(f, taint);
    fprintf(stderr, "%s v=0x%08x, t=0x%08x\n", REG_StringShort(r).c_str(), v, 
        taint);
  }
}

void TakeSnapshot(const CONTEXT *ctx, TaintTracker *tracker, context &delta, 
    const char *fn){
  u32 rcount, i, sz, offset;
  region_t *regions;
  region_t *r; 
  size_t size;
  FILE *f;
  uint8_t *dst;
  
  tracker->printRegs(delta);
  tracker->printMem();

  f = fopen(fn, "wb");
  if(!f){
    cerr << "ERROR: TakeSnapshot, can't open " << fn << endl;
    assert(false);
    return;
  }

  //TAG_START, format version
  EmitHeader(f);
  //TAG_REGS, [regid, value, taint]* [REG_INVALID_, 0, 0]
  EmitRegisters(f, ctx, delta);
  //TAG_TAINT, number of pairs, [address, taint info]* 
  EmitTaintMap(f, tracker->getMemory());

  regions = (region_t*)malloc(sizeof(region_t)*MAX_REGIONS);
  assert(regions != NULL);

  dst = (uint8_t*)malloc(COPY_SIZE);
  assert(dst != NULL);

  rcount = CollectRegions(ctx, regions);
  assert(rcount != 0);

  //TAG_REGIONS, number of regions, [low address, size]*
  EmitRegionTable(f, regions, rcount);

  for (i = 0; i < rcount; i++) {
    r = &regions[i];
    /* fprintf(stderr, "0x%08x - 0x%08x (0x%08x)\n", r->lo, r->hi, r->size); */
    sz = 0;
    for (offset = 0; offset < r->size; offset+=COPY_SIZE) {
      sz = MIN(r->size - offset, COPY_SIZE);
      size = ReadMem(dst, r->lo+offset, sz);
      assert(size == sz);

      size = fwrite(dst, 1, sz, f);
      assert(size == sz);
    }
  }
  free(dst);

  fclose(f);
}

