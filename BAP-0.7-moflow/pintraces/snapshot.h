#ifndef SNAPSHOT_H
#define SNAPSHOT_H

#define GET_DWORD(a,b,c,d) ((a<<24)|(b<<16)|(c<<8)|d)

#define PAGE_SIZE 4096
#define N_PAGES   (1<<(32-12))
//capture 10MB of stack space below (and everything above) esp 
//on linux x64, 32bit apps have 2GB stacks o_O
#define MAX_STACK (10*(1<<20)) 
//max. number of regions to capture
#define MAX_REGIONS 1024 
//copy to file in 1MB chunks
#define COPY_SIZE   1024*1024

//FIXME: Use protobuffers?
#define TAG_START       GET_DWORD('S','N','A','P')
#define FORMAT_VERSION  1
#define TAG_TAINT       GET_DWORD('T','M','A','P')
#define TAG_REGS        GET_DWORD('R','E','G','S')
#define TAG_REGIONS     GET_DWORD('R','G','N','S')

typedef uint32_t u32;

struct region {
  u32 lo;
  u32 hi;
  u32 size;
};

typedef struct region region_t;


extern void TakeSnapshot(const CONTEXT *, pintrace::TaintTracker *, \
    map<u32,u32> &, const char *);

#endif /* end of include guard: SNAPSHOT_H */
