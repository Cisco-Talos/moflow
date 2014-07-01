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
#include <string.h>
#include <cstring>

#include "pin_frame.h"
#include "pin_trace.h"
#include "reg_mapping_pin.h"
#include "cache.h"

/* The new trace container format */
#include "trace.container.hpp"

//#include "pin_frame.cpp"
//#include "pin_trace.cpp"

#include "pivot.h"

#include "pin_taint.h"
#include "snapshot.h"

using namespace pintrace;
using namespace SerializedTrace;

const ADDRINT ehandler_fs_offset = 0;
const ADDRINT ehandler_nptr_offset = 0;
const ADDRINT ehandler_handler_offset = 4;
const ADDRINT ehandler_invalid_ptr = 0xFFFFFFFF;
const ADDRINT ehandler_size = 8;
/** The offset esp has from when the exception is initially handled to
    when the handler is called. */
const ADDRINT ehandler_esp_offset = 0xe0;

const int maxSehLength = 10;

#ifdef _WIN32

const char* const windowsDll = "kernel32.dll";
const char* const wsDll = "WS2_32.dll";

const int callbackNum = 5;

const unsigned int accessViolation = 0xc0000005;

namespace WINDOWS {
#include "Winsock2.h"
#include "Windows.h"
}
#endif

/* Environment variables on windows
 * 
 * For a program that uses getenv, Windows does the following:
 *
 * 1. Call GetEnvironmentStringsW and set up an environment table.
 * 2. If using main (rather than wmain), WideCharToMultiByte is used
 * to convert to a multibyte environment table.
 * 
 * WideCharToMultiByte is implemented using a conversion table; we
 * don't handle control-flow taint, and thus we cannot really handle
 * tainting of environment variables :-/
 */

/* Networking on windows
 * 
 * Winsock appears to communicate with a windows subsystem using a
 * lightweight procedure calling interface, e.g., something we don't
 * want to parse.  So, we catch sockets() by instrumenting the socket
 * call itself.
 */

//
// CONFIGURATION
//

// Note: since we only flush the buffer once per basic block, the number
// of instructions per block should never exceed BUFFER_SIZE.
// TODO: See if there's some way to overcome this limitation, if
// necessary.
#define BUFFER_SIZE 10240

// Leave this much extra room in the frame buffer, for some unexpected
// frames.
#define FUDGE 5

// Add a keyframe every KEYFRAME_FREQ instructions.
#define KEYFRAME_FREQ 10240

// Use value caching.
#define USE_CACHING

// Use faster functions to append to the value buffer, where possible.
//#define USE_FASTPATH

#ifdef USE_FASTPATH
#define _FASTPATH true
#else
#define _FASTPATH false
#endif

/** Set to 1 to enable lock debug information */
#ifndef DEBUG_LOCK
#define DEBUG_LOCK 0
#endif

KNOB<string> KnobOut(KNOB_MODE_WRITEONCE, "pintool",
                     "o", "out.bpt",
                     "Trace file to output to.");

KNOB<int> KnobTrigAddr(KNOB_MODE_WRITEONCE, "pintool",
                       "trig_addr", "",
                       "Address of trigger point. No logging will occur until execution reaches this address.");

KNOB<string> KnobTrigModule(KNOB_MODE_WRITEONCE, "pintool",
                            "trig_mod", "",
                            "Module that trigger point is in.");

KNOB<int> KnobTrigCount(KNOB_MODE_WRITEONCE, "pintool",
                        "trig_count", "0",
                        "Number of times trigger will be executed before activating.");

//
// NOTE: This limit is not a hard limit; the generator only stops logging
// during buffer flushes, so the actual number of instructions logged
// might exceed log_limit, but at the most by BUFFER_SIZE.
// Also note that the limit is in terms of the number of _instructions_,
// not frames; things like keyframes, LoadModuleFrames, etc. are not
// included in the count.
//
KNOB<uint64_t> KnobLogLimit(KNOB_MODE_WRITEONCE, "pintool",
                            "log-limit", "0",
                            "Number of instructions to limit logging to.");
KNOB<bool> LogAllSyscalls(KNOB_MODE_WRITEONCE, "pintool",
                          "log-syscalls", "false",
                          "Log system calls (even those unrelated to taint)");

KNOB<bool> KnobCoverageTracking(KNOB_MODE_WRITEONCE, "pintool",
                               "coverage-track", "false", 
                             "Enable coverage tracking (only unique BBs)");

KNOB<string> KnobBBFile(KNOB_MODE_WRITEONCE, "pintool",
                               "bb-file", "", 
                             "Store the set of visited BBs as a text file");

KNOB<string> KnobExnFile(KNOB_MODE_WRITEONCE, "pintool",
                               "exn-file", "", 
                             "Store info about exception in a text file");

KNOB<string> KnobVisitedBBFile(KNOB_MODE_WRITEONCE, "pintool",
                               "visited-bb-file", "", 
                             "Do not log hits to BBs listed in this file");

KNOB<bool> KnobTaintTracking(KNOB_MODE_WRITEONCE, "pintool",
                             "taint-track", "true", 
                             "Enable taint tracking");

KNOB<bool> LogAllAfterTaint(KNOB_MODE_WRITEONCE, "pintool",
                            "logall-after", "false", 
                            "Log all (even untainted) instructions after the first tainted instruction");

KNOB<bool> LogAllBeforeTaint(KNOB_MODE_WRITEONCE, "pintool",
                             "logall-before", "false", 
                             "Log all (even untainted) instructions before and after the first tainted instruction");

// This option logs one instruction.  It then generates a fake
// standard frame to include operands after the instruction executed.
KNOB<bool> LogOneAfter(KNOB_MODE_WRITEONCE, "pintool",
                       "logone-after", "false",
                       "Log the first instruction outside of the log range (taint-start/end), and then exit.");

KNOB<bool> LogKeyFrames(KNOB_MODE_WRITEONCE, "pintool",
                        "log-key-frames", "false",
                        "Periodically output key frames containing important program values");

KNOB<string> TaintedFiles(KNOB_MODE_APPEND, "pintool",
                          "taint-files", "", 
                          "Consider the given files as being tainted");

KNOB<bool> TaintedArgs(KNOB_MODE_WRITEONCE, "pintool",
                       "taint-args", "false", 
                       "Command-line arguments will be considered tainted");

KNOB<bool> TaintedStdin(KNOB_MODE_WRITEONCE, "pintool",
                        "taint-stdin", "false", 
                        "Everything read from stdin will be considered tainted");

KNOB<bool> TaintedNetwork(KNOB_MODE_WRITEONCE, "pintool",
                          "taint-net", "false", 
                          "Everything read from network sockets will be considered tainted");

KNOB<bool> TaintedIndices(KNOB_MODE_WRITEONCE, "pintool",
                          "taint-indices", "false", 
                          "Values loaded with tainted memory indices will be considered tainted");

// FIXME: we should be able to specify more refined tainted 
// sources, e.g., that only the 5th argument should be considered
// tainted
KNOB<string> TaintedEnv(KNOB_MODE_APPEND, "pintool",
                        "taint-env", "", 
                        "Environment variables to be considered tainted");

KNOB<uint32_t> TaintStart(KNOB_MODE_WRITEONCE, "pintool",
                          "taint-start", "0x0", 
                          "All logged instructions will have higher addresses");

KNOB<uint32_t> TaintEnd(KNOB_MODE_WRITEONCE, "pintool",
                        "taint-end", "0xffffffff", 
                        "All logged instructions will have lower addresses");

KNOB<string> FollowProgs(KNOB_MODE_APPEND, "pintool",
                         "follow-progs", "", 
                         "Follow the given program names if they are exec'd");

KNOB<string> PivotFile(KNOB_MODE_WRITEONCE, "pintool",
                       "pivots-file", "",
                       "Load file of pivot gadgets");

KNOB<bool> SEHMode(KNOB_MODE_WRITEONCE, "pintool",
                   "seh-mode", "false",
                   "Record an SEH exploits");

KNOB<int> CheckPointFreq(KNOB_MODE_WRITEONCE, "pintool",
                         "freq", "10000",
                         "Report value of eip every n instructions.");

KNOB<int> CacheLimit(KNOB_MODE_WRITEONCE, "pintool",
                     "cache-limit", "500000000",
                     "Code-cache size limit (bytes)");

KNOB<int> SkipTaints(KNOB_MODE_WRITEONCE, "pintool",
                     "skip-taints", "0",
                     "Skip this many taint introductions");

KNOB<int> StackDump(KNOB_MODE_WRITEONCE, "pintool",
                     "stack-dump", "0",
                     "How many bytes of stack to dump on exception");

KNOB<string> SnapshotFile(KNOB_MODE_WRITEONCE, "pintool",
                     "snapshot-file", "",
                     "File name for memory snapshot");



struct FrameBuf {
    uint32_t addr;
    uint32_t tid;
    uint32_t insn_length;

    // The raw instruction bytes will be stored as 16 bytes placed over 4
    // integers. The conversion is equivalent to the casting of a char[16]
    // to a uint32_t[4].
    // NOTE: This assumes that MAX_INSN_BYTES == 16!!!
    uint32_t rawbytes0;
    uint32_t rawbytes1;
    uint32_t rawbytes2;
    uint32_t rawbytes3;

    uint32_t values_count;
    ValSpecRec valspecs[MAX_VALUES_COUNT];

};

/**
 * Temporary structure used during instrumentation.
 */
typedef struct TempOps_s {
    uint32_t reg;
    RegMem_t type;
    uint32_t taint;
} TempOps_t;

/**
 * Posible ways of passing a register to an analysis function
 */
enum RPassType { P_VALUE, P_REF, P_CONTEXT, P_FPX87 };

/**
 * Given a register, decide how to pass it.
 */
static RPassType howPass(REG r) {

    if(REG_is_fr_for_get_context(r))
      return P_CONTEXT;
    
    /* XMM and floating point registers can be passed by reference */
    if (REG_is_xmm(r) || REG_is_ymm(r) || REG_is_mm(r))
        return P_REF;

    if(REG_is_fr_or_x87(r))
        return P_FPX87;
    
    // For now, let's just use context
    return P_CONTEXT;
}

/**
 * Avoiding logging some addresses.
 */
static bool dontLog(ADDRINT addr) {

    IMG i = IMG_FindByAddress(addr);
    if (IMG_Valid(i)) {

        char tempbuf[BUFSIZE];
        char *tok = NULL;
        char *lasttok = NULL;
    
        // Fill up the temporary buffer
        strncpy(tempbuf, IMG_Name(i).c_str(), BUFSIZE);
    
        // We don't need a lock, since this is an instrumentation function (strtok is not re-entrant)
        strtok(tempbuf, "\\");
    
        while ((tok = strtok(NULL, "\\")) != NULL) {
            // Just keep parsing...
            lasttok = tok;
        }
    
        if (lasttok) {
            if (lasttok == string("uxtheme.dll")) {
                return true;
            }
        }
    }

    return false;
}



/**
 * This type preserves state between a system call entry and exit.
 */
typedef struct SyscallInfo_s {

    /** Frame for system call */
    frame sf;

    /** State shared between taintIntro and taintStart */
    uint32_t state;
} SyscallInfo_t;

/**
 * This type preserves state between a recv() call and return
 */
typedef struct RecvInfo_s {
    /** Fd */
    uint32_t fd;

    /** The address */
    void* addr;

    /** Bytes written ptr. */
    uint32_t *bytesOut;
} RecvInfo_t;

/**
 * Thread local information
 */

typedef struct ThreadInfo_s {
    // Stack keeping track of system calls
    // Needed because windows system calls can be nested!
    std::stack<SyscallInfo_t> scStack;
    std::stack<RecvInfo_t> recvStack;
    context delta;
} ThreadInfo_t;

int g_counter = 0;

//TraceWriter *g_tw;
TraceContainerWriter *g_twnew;

////////////////////////////////////////////////////////////
// Coverage tracing stuff 
////////////////////////////////////////////////////////////
// True if we are logging code coverage (unoptimized, stored in .bpt)
bool g_logCoverage;
// Save BBs to this file.
FILE *g_bbfile = NULL;
// BBs read from this file are not traced.
FILE *g_visited_bb_file = NULL;
// Set of BBs which were visited during previous traces.
set<ADDRINT> g_visited_bbs;
// Optimized BB format: set of addresses of BBs (no dupes).
set<ADDRINT> g_bbs;
PIN_LOCK g_bb_lock;
/// Types
typedef std::map<std::string, std::pair<ADDRINT, ADDRINT> > MODULE_BLACKLIST_T;
typedef MODULE_BLACKLIST_T MODULE_LIST_T;
// This is the list of the blacklisted modules.
// You can find their names & start/end addresses.
MODULE_BLACKLIST_T modules_blacklisted;
////////////////////////////////////////////////////////////


// A taint tracker
TaintTracker * tracker;

FrameBuf g_buffer[BUFFER_SIZE];
uint32_t g_bufidx;

// Counter to keep track of when we should add a keyframe.
uint32_t g_kfcount;

// Caches.
//RegCache g_regcache;
//MemCache g_memcache;

// Profiling timer.
clock_t g_timer;

// True if logging is activated.
// Logging should be activated if it is possible for some instruction
// to be logged.  This could happen because 1) we are logging all
// instructions, or 2) taint is introduced, and so the instruction
// could be tainted.
bool g_active;

// Number of instructions logged so far.
uint64_t g_logcount;

// Number of instructions to limit logging to.
uint64_t g_loglimit;

// True if a trigger was specified.
bool g_usetrigger;

// Activate taint analysis
// bool t_active;

// Whether taint has been introduced
bool g_taint_introduced;

// True if the trigger address was resolved.
bool g_trig_resolved;

uint32_t g_trig_addr;

// We use a signed integer because sometimes the countdown will be
// decremented past zero.
int g_trig_countdown;

// Should we dump stack contents on exception?
// Needed for forward symbolic execution
int g_stack_dump;

// Should we take a memory snapshot on exception?
// Needed for forward symbolic execution
int g_mem_snapshot;

// Name of our thread/process
char g_threadname[BUFFER_SIZE] = "s";

// A lock on any shared state
PIN_LOCK lock;

// An environment to keep all the values
ValSpecRec values[MAX_VALUES_COUNT];

// Address ranges
uint32_t start_addr, end_addr;

// Pivot set
pivot_set ps;

// Exit after the next instruction
bool g_exit_next;

// Prototypes.
VOID Cleanup();

// Key for thread local system call stack
static TLS_KEY tl_key;


// Start of functions.

VOID ModLoad(IMG i, void*);

VOID PIN_FAST_ANALYSIS_CALL LogBlockHit(THREADID tid, ADDRINT address);
VOID PIN_FAST_ANALYSIS_CALL LogFunctionCall(THREADID tid, ADDRINT address, ADDRINT target);
VOID PIN_FAST_ANALYSIS_CALL LogFunctionRet(THREADID tid, ADDRINT address, ADDRINT target);

// Get Thread Info
ThreadInfo_t* GetThreadInfo(void) {
    ThreadInfo_t* ti;

    ti = static_cast<ThreadInfo_t*> (PIN_GetThreadData(tl_key, PIN_ThreadId()));
    assert(ti);
    return ti;
}

// Create a new thread information block for the current thread
ThreadInfo_t* NewThreadInfo(void) {
    ThreadInfo_t* ti = NULL;

    ti = new ThreadInfo_t;
    assert(ti);

    PIN_SetThreadData(tl_key, ti, PIN_ThreadId());

    return ti;
}

/** Given a REG, return the number of bits in the reg */
static uint32_t GetBitsOfReg(REG r) {
    if (REG_is_gr8(r)) return 8;
    if (REG_is_gr16(r)) return 16;
    if (REG_is_gr32(r)) return 32;
    if (REG_is_gr64(r)) return 64;

    /* REG_is_fr_or_x87 returns true on XMM registers and other
       non-x87 regs, so we can't use that. */
    if (LEVEL_BASE::REG_ST_BASE <= r && r <= LEVEL_BASE::REG_ST_LAST) return 80;

    string s = REG_StringShort(r);

    switch (r) {
    case LEVEL_BASE::REG_SEG_CS:
    case LEVEL_BASE::REG_SEG_DS:
    case LEVEL_BASE::REG_SEG_ES:
    case LEVEL_BASE::REG_SEG_FS:
    case LEVEL_BASE::REG_SEG_GS:
    case LEVEL_BASE::REG_SEG_SS:
        return 16;
        break;

    case LEVEL_BASE::REG_INST_PTR:
    case LEVEL_BASE::REG_EFLAGS:
    case LEVEL_BASE::REG_MXCSR:
        return 32;
        break;

    case LEVEL_BASE::REG_MM0:
    case LEVEL_BASE::REG_MM1:
    case LEVEL_BASE::REG_MM2:
    case LEVEL_BASE::REG_MM3:
    case LEVEL_BASE::REG_MM4:
    case LEVEL_BASE::REG_MM5:
    case LEVEL_BASE::REG_MM6:
    case LEVEL_BASE::REG_MM7:
        return 64;
        break;

    case LEVEL_BASE::REG_XMM0:
    case LEVEL_BASE::REG_XMM1:
    case LEVEL_BASE::REG_XMM2:
    case LEVEL_BASE::REG_XMM3:
    case LEVEL_BASE::REG_XMM4:
    case LEVEL_BASE::REG_XMM5:
    case LEVEL_BASE::REG_XMM6:
    case LEVEL_BASE::REG_XMM7:
        return 128;
        break;

    case LEVEL_BASE::REG_YMM0:
    case LEVEL_BASE::REG_YMM1:
    case LEVEL_BASE::REG_YMM2:
    case LEVEL_BASE::REG_YMM3:
    case LEVEL_BASE::REG_YMM4:
    case LEVEL_BASE::REG_YMM5:
    case LEVEL_BASE::REG_YMM6:
    case LEVEL_BASE::REG_YMM7:
        return 256;
        break;

    default:
        break;
    }

    // Otherwise, exit because we don't know what's up
    cerr << "Warning: Unknown register size of register " << REG_StringShort(r) << endl;
    assert(false);
    return -1;
}

static uint32_t GetByteSize(RegMem_t vtype) {
    return (vtype.size / 8);
}

static uint32_t GetBitSize(RegMem_t type) {
    return type.size;
}

void LLOG(const char *str) {
#if DEBUG_LOCK
    LOG(str);
#else
    /* Disabled */
#endif
}

ADDRINT CheckTrigger()
{
    return --g_trig_countdown <= 0;
}

/** Reinstrument all images. XXX: Remove me. */
VOID InstrumentIMG() {
    PIN_LockClient();
    for (IMG i = APP_ImgHead(); IMG_Valid(i); i = IMG_Next(i)) {
        ModLoad(i, (void*)1);
    }
    PIN_UnlockClient();
}

VOID Activate(CONTEXT *ctx)
{
    cerr << "Activating logging" << endl;
    g_active = true;
    PIN_RemoveInstrumentation();
    PIN_ExecuteAt(ctx);
}

/** Activate taint analysis.

    Note: It's important to NOT hold locks when calling this function.
    PIN_RemoveInstrumentation obtains the VM lock, which is only possible
    when no analysis functions/etc are executing.  If one is waiting for
    one of our locks, this will cause a deadlock.  
*/
VOID TActivate()
{
    cerr << "Activating taint analysis " << endl;
    g_active = true; /* Any instruction could be logged because taint is
                        introduced. */
    g_taint_introduced = true; /* Taint is definitely introduced now. */
    PIN_RemoveInstrumentation();
    InstrumentIMG();
}

//
// Returns true if the buffer index with count added to it exceeds the
// maximum size of the buffer.
//
ADDRINT CheckBuffer(UINT32 count)
{
  return (g_bufidx + count) >= BUFFER_SIZE - FUDGE;
}

ADDRINT CheckBufferEx(BOOL cond, UINT32 count, UINT32 count2)
{
  return cond && ((g_bufidx + count + count2) >= BUFFER_SIZE - FUDGE);
}

//////////////////////////////////////////////////////////////
// Dump stack contents as a series of movs for easier 
// processing. This should be packed as a new frame type.
//////////////////////////////////////////////////////////////
// movs are synthetic, so they don't have an address
#define FAKE_ADDR 0
// c7 84 24 xx xx xx xx yy yy yy yy
// mov dword ptr [esp+xxxxxxxx], yyyyyyyy
#define MOV_DWORD_ESP_LEN 11 
// ExceptionHandler takes the lock
VOID AppendStackDumping(THREADID tid, const CONTEXT *ctx){
  int i, offset, fix, dwords_to_dump;
  uint32_t *val_ptr, *off_ptr;
  FrameBuf *fb;
  ValSpecRec *vsr;
  static const RegMem_t REG_ESP_TYPE = {REGISTER, 32};
  static const RegMem_t MEM_U8_TYPE = {MEM, 8};
  unsigned char mov_esp_bytes[MOV_DWORD_ESP_LEN] = { 
    0xc7, 0x84, 0x24,       //+0, opcode
    0x00, 0x00, 0x00, 0x00, //+3, offset
    0x00, 0x00, 0x00, 0x00  //+7, value
  };
  ThreadInfo_t *ti = NULL;

  ti = GetThreadInfo();

  ADDRINT esp = PIN_GetContextReg(ctx, REG_STACK_PTR);
  offset = 0;
  off_ptr = (uint32_t*)(mov_esp_bytes+3);
  val_ptr = (uint32_t*)(mov_esp_bytes+7);
  dwords_to_dump = (g_stack_dump+3)/4;

  for(i=0;i<dwords_to_dump;i++){
    assert (g_bufidx < BUFFER_SIZE);
        
    fb = &g_buffer[g_bufidx];
    fb->addr = FAKE_ADDR;
    fb->tid = tid;
    fb->insn_length = MOV_DWORD_ESP_LEN;

    *off_ptr = offset;
    PIN_SafeCopy((VOID*)val_ptr, (const VOID *)(esp+offset), 4); 
    // mov [esp+offset], val
    fb->rawbytes0 = *(uint32_t*)mov_esp_bytes;
    fb->rawbytes1 = *(uint32_t*)(mov_esp_bytes+4);
    fb->rawbytes2 = *(uint32_t*)(mov_esp_bytes+8);
    fb->rawbytes3 = 0;

    fb->values_count = 5; //reg+4 mems

    //opnd esp (reg)
    vsr = fb->valspecs;
    vsr->type = REG_ESP_TYPE;
    vsr->usage = RD;
    vsr->loc = REG_STACK_PTR;
    vsr->taint = tracker->getRegTaint(ti->delta, REG_STACK_PTR);
    vsr->value.dword[0] = esp;
    //opnd [esp+offset] (mem)
    for(fix=0;fix<4;fix++){
      vsr++;
      vsr->type = MEM_U8_TYPE;
      vsr->usage = WR;
      vsr->loc = esp+offset+fix;
      //0 - no taint
      //n - nth input byte
      //-1 - mixed taint (for example 1st+2nd byte -> mixed)
      vsr->taint = tracker->getMemTaint(vsr->loc, vsr->type);
      PIN_SafeCopy((VOID*) &(vsr->value), 
          (const VOID *)(vsr->loc), 
          1); //byte size
    }
    offset += 4; //next dword
    g_bufidx++;
  }
}



// Callers must ensure mutual exclusion
VOID FlushInstructions()
{

    for(uint32_t i = 0; i < g_bufidx; i++) {

        frame fnew;
        fnew.mutable_std_frame()->set_address(g_buffer[i].addr);
        fnew.mutable_std_frame()->set_thread_id(g_buffer[i].tid);
        /* Ew. */
        fnew.mutable_std_frame()->set_rawbytes((void*)(&(g_buffer[i].rawbytes0)), g_buffer[i].insn_length);

        /* Add operands */

        // Go through each value and remove the ones that are cached.

        /* The operand_list is a required field, so we must access it
           even if there are no operands or protobuffers will complain to
           us. */
        fnew.mutable_std_frame()->mutable_operand_list();

        for (uint32_t j = 0; j < g_buffer[i].values_count; j++) {

            ValSpecRec &v = g_buffer[i].valspecs[j];

            operand_info *o = fnew.mutable_std_frame()->mutable_operand_list()->add_elem();
            o->set_bit_length(GetBitSize(v.type));
            o->mutable_operand_usage()->set_read(v.usage & RD);
            o->mutable_operand_usage()->set_written(v.usage & WR);
            /* XXX: Implement index and base */
            o->mutable_operand_usage()->set_index(false);
            o->mutable_operand_usage()->set_base(false);

            switch (v.taint) {
            case 0:
                o->mutable_taint_info()->set_no_taint(true);
                break;
            case -1:
                o->mutable_taint_info()->set_taint_multiple(true);
                break;
            default:
                o->mutable_taint_info()->set_taint_id(v.taint);
                break;
            }

            if (tracker->isMem(v.type)) {
                o->mutable_operand_info_specific()->mutable_mem_operand()->set_address(v.loc);

            } else {
                string t = pin_register_name((REG)v.loc);
                if (t == "Unknown") {
                    t = string("Unknown ") + REG_StringShort((REG)v.loc);
                }
                o->mutable_operand_info_specific()->mutable_reg_operand()->set_name(t);
            }

            o->set_value(&(v.value), GetByteSize(v.type));

            // We're in trouble if we don't know the type.
            if(v.type.type != REGISTER && v.type.type != MEM) {
                cerr << "v.type = " << v.type.type << endl;                
                assert(false);
            }
        }

        g_twnew->add(fnew);
    }

    // Update counts.
    g_logcount += g_bufidx;
    g_kfcount += g_bufidx;

    g_bufidx = 0;

}

/* Add a PIN register to a value list. Helper function for FlushBuffer */
VOID AddRegister(tagged_value_list *tol, const CONTEXT *ctx, REG r, THREADID threadid) {
    tol->mutable_value_source_tag()->set_thread_id(threadid);
    value_info *v = tol->mutable_value_list()->add_elem();
    v->mutable_operand_info_specific()->mutable_reg_operand()->set_name(REG_StringShort(r));
      size_t s_bytes = GetBitsOfReg(r) / 8;
      v->set_bit_length(s_bytes * 8);

    /* Make sure this register even fits in the context.  PIN would
       probably throw an error, but it's good to be paranoid. */
    assert (s_bytes <= sizeof(ADDRINT));
    ADDRINT regv = PIN_GetContextReg(ctx, r);
    v->set_value((void*)(&regv), s_bytes);
    //std::copy((uint8_t*) (&regv), ((uint8_t*) (&regv)) + s_bytes, v->
}

//
// Writes all instructions stored in the buffer to disk, and resets the
// buffer index to 0. Also checks to see if we need to insert a
// keyframe. If so, inserts the keyframe using the data in the supplied
// context.
//
VOID FlushBuffer(BOOL addKeyframe, const CONTEXT *ctx, THREADID threadid, BOOL needlock)
{

    LLOG("Begin flushing buffer.\n");

    if (needlock) {
        GetLock(&lock, threadid+1);
    }

    FlushInstructions();


    // Check to see if we should insert a keyframe here.
    if (addKeyframe && (g_kfcount >= KEYFRAME_FREQ) && LogKeyFrames) {

        //LOG("Inserting keyframe:\n");
        //LOG("  addr: " + hexstr(PIN_GetContextReg(ctx, LEVEL_BASE::REG_EIP)) + "\n");

        assert(ctx);

        frame f;
        tagged_value_list *tol = f.mutable_key_frame()->mutable_tagged_value_lists()->add_elem();
        AddRegister(tol, ctx, LEVEL_BASE::REG_EAX, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_EBX, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_ECX, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_EDX, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_ESI, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_EDI, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_ESP, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_EBP, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_EFLAGS, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_SEG_CS, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_SEG_DS, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_SEG_SS, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_SEG_ES, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_SEG_FS, threadid);
        AddRegister(tol, ctx, LEVEL_BASE::REG_SEG_GS, threadid);

        g_twnew->add(f);

        g_kfcount = 0;

    }

    // See if we've gotten sufficient instructions.
    if ((g_loglimit != 0) && (g_logcount >= g_loglimit)) {
        LOG("Logged required number of instructions, quitting.\n");
        cerr << "Logged required number of instructions, quitting." << endl;
        Cleanup();
        //ReleaseLock(&lock);
        // Never release lock
        //PIN_Detach();
        exit(0);
    } else {
        if (needlock) {
            ReleaseLock(&lock);
        }
    }


   
    LLOG("End flushing buffer.\n");

}

void PIN_FAST_ANALYSIS_CALL LogBlockHit(THREADID tid, ADDRINT address)
{
  int written;
  char *fmt;
  set<ADDRINT>::iterator it;

  GetLock(&g_bb_lock, tid+1);
  it = g_bbs.find(address);
  if(it != g_bbs.end()){
    ReleaseLock(&g_bb_lock);
    return;
  }
  g_bbs.insert(address);
  ReleaseLock(&g_bb_lock);

	//cerr << "This is LogBlockHit()" << endl;

  GetLock(&lock, tid+1);
  if(g_logCoverage){
    frame f;
    f.mutable_block_frame()->set_address(address);
    f.mutable_block_frame()->set_thread_id(tid);

    g_twnew->add(f);
  }
  //FIXME: move this to exit function
  else if(g_bbfile){
    if(sizeof(ADDRINT) == 4){
      fmt = (char*)"0x%lx\n";
    }
    else if(sizeof(ADDRINT) == 8){
      fmt = (char*)"0x%llx\n";
    }
    else{
      assert(FALSE);
    }
    written = fprintf(g_bbfile, fmt, address);
    assert(written >=0);
    fflush(g_bbfile);
  }
  else{
    assert(FALSE);
  }
  ReleaseLock(&lock);
}

void PIN_FAST_ANALYSIS_CALL LogFunctionCall(THREADID tid, ADDRINT address, ADDRINT target)
{
	//cerr << "This is LogFunctionCall()" << endl;

	frame f;
	f.mutable_call_frame()->set_address(address);
	f.mutable_call_frame()->set_target(target);
	f.mutable_call_frame()->set_thread_id(tid);

  GetLock(&lock, tid+1);
	g_twnew->add(f);
  ReleaseLock(&lock);
}

void PIN_FAST_ANALYSIS_CALL LogFunctionRet(THREADID tid, ADDRINT address, ADDRINT target)
{
	//cerr << "This is LogFunctionRet()" << endl;

	frame f;
	f.mutable_ret_frame()->set_address(address);
	f.mutable_ret_frame()->set_target(target);
	f.mutable_ret_frame()->set_thread_id(tid);

  GetLock(&lock, tid+1);
	g_twnew->add(f);
  ReleaseLock(&lock);
}


#ifdef _WIN32

/** Wrapper for accept */
uint32_t AcceptWrapper(CONTEXT *ctx, AFUNPTR fp, THREADID tid, uint32_t s, void *addr, int *addrlen) {

    cerr << "AcceptWrapper" << endl;

    uint32_t ret;

    PIN_CallApplicationFunction(ctx, tid,
                                CALLINGSTD_STDCALL, fp,
                                PIN_PARG(uint32_t), &ret,
                                PIN_PARG(uint32_t), s,
                                PIN_PARG(void*), addr,
                                PIN_PARG(int*), addrlen,
                                PIN_PARG_END());

    GetLock(&lock, tid+1);
    tracker->acceptHelper(ret);
    ReleaseLock(&lock);

    return ret;
			      
}

/** Wrapper for WSAConnect */
uint32_t WSAConnectWrapper(CONTEXT *ctx, AFUNPTR fp, THREADID tid, uint32_t s, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7) {

    cerr << "WSAConnectWrapper" << endl;

    uint32_t ret;

    cerr << "Connect to socket " << s << endl;

    PIN_CallApplicationFunction(ctx, tid,
                                CALLINGSTD_STDCALL, fp,
                                PIN_PARG(uint32_t), &ret,
                                PIN_PARG(uint32_t), s,
                                PIN_PARG(void*), arg2,
                                PIN_PARG(void*), arg3,
                                PIN_PARG(void*), arg4,
                                PIN_PARG(void*), arg5,
                                PIN_PARG(void*), arg6,
                                PIN_PARG(void*), arg7,
                                PIN_PARG_END());

    GetLock(&lock, tid+1);
    if (ret != SOCKET_ERROR) {
        tracker->acceptHelper(s);
    } else {
        cerr << "WSAConnect error " << ret << endl;
    }
    ReleaseLock(&lock);

    return ret;
			      
}

/** Wrapper for connect */
uint32_t ConnectWrapper(CONTEXT *ctx, AFUNPTR fp, THREADID tid, uint32_t s, void *arg2, void *arg3) {

    cerr << "ConnectWrapper" << endl;

    uint32_t ret;

    cerr << "Connect to socket " << s << endl;

    PIN_CallApplicationFunction(ctx, tid,
                                CALLINGSTD_STDCALL, fp,
                                PIN_PARG(uint32_t), &ret,
                                PIN_PARG(uint32_t), s,
                                PIN_PARG(void*), arg2,
                                PIN_PARG(void*), arg3,
                                PIN_PARG_END());

    GetLock(&lock, tid+1);
    //  if (ret != SOCKET_ERROR) {
    // Non-blocking sockets will return an "error".  However, we can't
    // call GetLastError to find out what the root problem is,
    // so... we'll just assume the connection was successful.
    tracker->acceptHelper(s);
 
    // } else {
    //    cerr << "connect error " << ret << endl;
    //  }
    ReleaseLock(&lock);

    return ret;
			      
}

void BeforeRecv(THREADID tid, uint32_t s, char* buf) {

    RecvInfo_t r;

    r.fd = s;
    r.addr = buf;
    r.bytesOut = NULL;

    ThreadInfo_t *ti = GetThreadInfo();

    ti->recvStack.push(r);
}

void WSABeforeRecv(THREADID tid, uint32_t s, WINDOWS::LPWSABUF bufs, WINDOWS::LPDWORD bytesOut) {

    RecvInfo_t r;

    r.fd = s;
    r.addr = bufs[0].buf;
    r.bytesOut = (uint32_t*) bytesOut;

    ThreadInfo_t *ti = GetThreadInfo();

    ti->recvStack.push(r);
}

void AfterRecv(THREADID tid, int ret, char *f) {
    cerr << "afterrecv called by " << f << endl;
    ThreadInfo_t *ti = GetThreadInfo();
    uint32_t len = 0;

    if (ti->recvStack.empty()) {
        cerr << "WARNING: Stack empty in AfterRecv(). Thread " << tid << endl;
    } else {
  
        RecvInfo_t ri = ti->recvStack.top();
        ti->recvStack.pop();
    
        if (ret != SOCKET_ERROR) {
            GetLock(&lock, tid+1);
            //cerr << "fd: " << ri.fd << endl;

            uint32_t numbytes = 0;
            if (ri.bytesOut) {
                numbytes = *(ri.bytesOut);
            } else {
                numbytes = ret;
            }

            FrameOption_t fo = tracker->recvHelper(ri.fd, ri.addr, numbytes);
            ReleaseLock(&lock);
      
            if (fo.b) {
	
                if (!g_taint_introduced) {
                    TActivate();
                }
	
                GetLock(&lock, tid+1);
                g_twnew->add(fo.f);
                ReleaseLock(&lock);
            }
        } else {
            cerr << "recv() error " << endl;
        }
    }
}


/** Wrapper for calling GetEnvironmentStringsW() and tainting the output */
void* GetEnvWWrap(CONTEXT *ctx, AFUNPTR fp, THREADID tid) {
    void *ret = NULL;

    /* 
       We must lock after the PIN_CallApplicationFunction call, since
       the called code is instrumented, and also tries to obtain the
       lock. 
     
       This probably is not a big deal, but theoretically the
       instrumented code in another thread could change the memory as
       we're reading it.  This seems pretty unlikely.  If we ever feel
       like fixing it, we could obtain PIN's global vm lock.
    */

    PIN_CallApplicationFunction(ctx, tid,
                                CALLINGSTD_STDCALL, fp,
                                PIN_PARG(uint32_t), &ret,
                                PIN_PARG_END());

    LLOG("Getting lock in callback\n");

    GetLock(&lock, tid+1);
    LLOG("Got callback lock\n");

    std::vector<frame> frms = tracker->taintEnv(NULL, (wchar_t*) ret);
    g_twnew->add<std::vector<frame> > (frms);

    ReleaseLock(&lock);
    LLOG("Releasing callback lock\n");

    return ret;
}

/** Wrapper for calling GetEnvironmentStringsA() and tainting the output */
void* GetEnvAWrap(CONTEXT *ctx, AFUNPTR fp, THREADID tid) {
    void *ret = NULL;

    cerr << "In a wrap " << endl;

    /* 
       We must lock after the PIN_CallApplicationFunction call, since
       the called code is instrumented, and also tries to obtain the
       lock. 
     
       This probably is not a big deal, but theoretically the
       instrumented code in another thread could change the memory as
       we're reading it.  This seems pretty unlikely.  If we ever feel
       like fixing it, we could obtain PIN's global vm lock.
    */

    PIN_CallApplicationFunction(ctx, tid,
                                CALLINGSTD_STDCALL, fp,
                                PIN_PARG(uint32_t), &ret,
                                PIN_PARG_END());

    LLOG("Getting lock in callback\n");

    GetLock(&lock, tid+1);
    LLOG("Got callback lock\n");

    std::vector<frame> frms = tracker->taintEnv((char*) ret, NULL);
    g_twnew->add<std::vector<frame> > (frms);

    ReleaseLock(&lock);
    LLOG("Releasing callback lock\n");

    return ret;
}

#endif

/* This analysis function is called after the target instruction is
 * executed when using -logone-after. It transfer control back to the
 * same instruction to log its operands after execution. */
VOID PostInstruction(ADDRINT addr, CONTEXT *ctx) {
    g_exit_next = true;
    PIN_SetContextReg(ctx, LEVEL_BASE::REG_INST_PTR, addr);
    PIN_ExecuteAt(ctx);
}

VOID AppendBuffer(ADDRINT addr,
                  THREADID tid,
                  CONTEXT *ctx,
                  BOOL isBranch,
                  UINT32 insn_length,

                  UINT32 rawbytes0,
                  UINT32 rawbytes1,
                  UINT32 rawbytes2,
                  UINT32 rawbytes3,
                   
                  /* Type contains the type of the operand. Location
                   * specifies the base address for memory operands.
                   * For registers, this holds the ID of the
                   * register.  Value is used to pass register values
                   * by reference.  For memory operands, it holds the
                   * byte offset into memory.  For instance, a 32-bit
                   * memory operand is broken into four 8-bit operands
                   * with the same address (specified in location),
                   * but with different offsets (0, 1, 2, 3) in
                   * value.  Usage specifies how the operand is used
                   * (read, write, etc.) */                   

                  UINT32 values_count,
                  ...
                  )
{
    va_list va;
    va_start(va, values_count);

    static int firstTaint = true;
    static int firstLogged = true;
    REG r;

    //LOG("APPEND: " + hexstr(addr) + "\n");

    /* BUILD_VAL touches values, so we need the lock early. */

    /* Periodically report eip. */
    if ((g_counter++ % CheckPointFreq.Value()) == 0) {
        // PIN_LockClient();
        // IMG i = IMG_FindByAddress(addr);
        // PIN_UnlockClient();
        // cerr << "Checkpoint: Executing code at " << addr;
        // if (IMG_Valid(i)) {
        //   cerr << " (" << IMG_Name(i) << ")";
        // } 
        cerr << " thread " << tid
             << "; " << g_counter << " instructions" << endl
             << "Code cache size is " << CODECACHE_CodeMemUsed() << endl
             << "Code cache limit is " << CODECACHE_CacheSizeLimit() << endl;
    }

    LLOG("big thing\n");
  
    GetLock(&lock, tid+1);
  
    LLOG("got big thing\n");

    for (unsigned int i = 0; i < values_count; i++) {
        values[i].type.type = (RegMemEnum_t)va_arg(va, uint32_t);
        assert(valid_regmem_type(values[i].type));
        
        values[i].type.size = va_arg(va, uint32_t);
        values[i].loc = va_arg(va, uint32_t);
        values[i].value.dword[0] = va_arg(va, uint32_t);			
        values[i].usage = va_arg(va, uint32_t);				
        if (tracker->isMem(values[i].type)) {
            /* Add memory byte offset */					
            values[i].loc += values[i].value.dword[0];			
        } 		    
    }

    /* Perform taint propagation and checking */

    bool abort = false;
    bool log_addr =
        ((start_addr <= addr) && (addr <= end_addr)) || LogOneAfter.Value();
    bool log_all =
        ((LogAllAfterTaint.Value() && !firstTaint)
         || LogAllBeforeTaint.Value());
    uint32_t pretaint[MAX_VALUES_COUNT];
    LEVEL_VM::PIN_REGISTER *pr = NULL;
    ThreadInfo_t *ti = NULL;

    ti = GetThreadInfo();

    tracker->setCount(values_count);

    bool has_taint = tracker->hasTaint(ti->delta);

    if ((log_all || has_taint) && log_addr) {

        /* This instruction is tainted, or we're logging all
         * instructions */

        if (firstLogged) {
            cerr << "First logged instruction" << endl;
            firstLogged = false;
        }
     
        if (has_taint && firstTaint) {
            cerr << "First tainted instruction" << endl;
            LOG("First tainted instruction.\n");
            firstTaint = false;
        }

        // Mark everything as untainted
        for (uint32_t i = 0 ; i < values_count ; i++) 
            values[i].taint = 0;
     
        // Set taint values from taint context
        tracker->setTaintContext(ti->delta);

        // Record pretaint (this goes in the log)
        for (uint32_t i = 0 ; i < values_count ; i++) 
            pretaint[i] = values[i].taint;

        // Did this instruction propagate taint?
        //propagated_taint = tracker->propagatedTaint(isBranch);
      
        if (!isBranch)
            tracker->taintPropagation(ti->delta);

        // Taint checking
        abort = !tracker->taintChecking();
         
        //} FIXME: it there a case where the instruction contains taint
        //  but we do not need to log it?
        //if (log || (has_taint && propagated_taint)) {
   
        //cerr << "Logging instruction " << rawbytes0 << " " << rawbytes1 << endl;

        // Now, fill in the buffer with information

	assert (g_bufidx < BUFFER_SIZE);
      
        g_buffer[g_bufidx].addr = addr;
        g_buffer[g_bufidx].tid = tid;
        g_buffer[g_bufidx].insn_length = insn_length;

        g_buffer[g_bufidx].rawbytes0 = rawbytes0;
        g_buffer[g_bufidx].rawbytes1 = rawbytes1;
        g_buffer[g_bufidx].rawbytes2 = rawbytes2;
        g_buffer[g_bufidx].rawbytes3 = rawbytes3;

        // tracker->printRegs();

        g_buffer[g_bufidx].values_count = values_count;
  
        //g_buffer[g_bufidx].valspecs[i].taint = values[i].taint;             

        // Values for floating point operations
        bool got_FP_state = false;
        FPSTATE fpState;
        void * fpValue;
        uint32_t s_i;
        
        // Store information to the buffer
        for (unsigned int i = 0; i < values_count; i++) {

            g_buffer[g_bufidx].valspecs[i].type = values[i].type;		
            g_buffer[g_bufidx].valspecs[i].usage = values[i].usage;		
            g_buffer[g_bufidx].valspecs[i].loc = values[i].loc;			
            g_buffer[g_bufidx].valspecs[i].taint = pretaint[i];
     
            if(values[i].type.type == REGISTER) {						
                       /*r = LEVEL_BASE::REG_FullRegName((REG) valspec##i##_loc);*/		
                r = (REG)values[i].loc;
       
                /* Find how we should access the register value */             
                switch(howPass(r)) {                                        
                case P_CONTEXT:
                    g_buffer[g_bufidx].valspecs[i].value.dword[0] =
                        PIN_GetContextReg(ctx, r);
                    break;
         
                case P_REF:
                    pr = (LEVEL_VM::PIN_REGISTER*) values[i].value.dword[0];	
                    memcpy(&(g_buffer[g_bufidx].valspecs[i].value),
                           pr,                                               
                           sizeof(LEVEL_VM::PIN_REGISTER));                  
                    break;                                                   

                case P_FPX87:
                    if(!got_FP_state) {
                        /* This is relativly expensive, so only do it once per
                           instruction */
                        PIN_GetContextFPState(ctx, &fpState);
                        got_FP_state = true;
                    }

                    // Figure out which st register we are using
                    s_i = r - LEVEL_BASE::REG_ST_BASE;
                    
                    if (s_i > 7) {
                        cerr << "Unknown FP register " << r << " at addr "
                             << addr << endl;
                        assert(false);
                    }
                    
                    fpValue = (void *)&(fpState.fxsave_legacy._sts[s_i]);
                                        
                    memcpy(&(g_buffer[g_bufidx].valspecs[i].value.flt[0]),
                           fpValue,
                           10); // FP are 80 bits = 10 bytes
                    break;
                    
                default:                                                   
                    assert(false);                                           
                    break;                                                   
                }                                                              
            } else if(values[i].type.type == MEM) {
                PIN_SafeCopy((VOID*) &(g_buffer[g_bufidx].valspecs[i].value),	
                             (const VOID *)(g_buffer[g_bufidx].valspecs[i].loc),
                             GetByteSize(values[i].type));
            } else {
                cerr << "Unknown operand type at addr "        
                     << addr << endl;                                          
                assert(false);                                                 
            }
            
            //   cerr << "Building val specs now" << endl;
   
        }

        //   cerr << "... done" << endl;
   
        g_bufidx++;
    }

   
    /* For a non-SEH exploit, stop if taint checking fails.  In an SEH
       exploit, we may want an exception to trigger (e.g., from
       returning to a bad address). */
    if (abort && !SEHMode.Value()) { 
        pivot_set::iterator i;
        cerr << "Stack smashing detected! @" << addr << endl;
        cerr << "Exiting...." << endl;
        LOG("Stack smashing detected\n");

        /* Let's assume this was a ret for now and increment esp by
         * four.  Of course, this isn't correct in general.  XXX: Fix
         * this so it works for any last instruction. */
        ADDRINT esp = PIN_GetContextReg(ctx, LEVEL_BASE::REG_STACK_PTR);
        PIN_SetContextReg(ctx, LEVEL_BASE::REG_STACK_PTR, esp+4);
     
        PIVOT_testpivot(ps, ctx, *tracker);
     
        FlushBuffer(true, ctx, tid, false);
        Cleanup();
        exit(0);
    }

    if (g_exit_next) {
        FlushBuffer(true, ctx, tid, false);
        Cleanup();
        exit(0);
    }
   
    ReleaseLock(&lock);  
    LLOG("released big thing\n");

    va_end(va);

    return;

}

VOID InstrBlock(BBL bbl)
{

    // Now we need to get the values.
  
    uint32_t valcount;
    uint32_t icount = BBL_NumIns(bbl);
  
    // Used to temporarily store the values we obtain from the operands,
    // to faciliate further analysis for fast paths.
    TempOps_t opndvals[MAX_VALUES_COUNT];
  

    // LOG("INS: BBL start.\n");
		

    if (g_active) {

        if (icount > BUFFER_SIZE) {
            LOG("WARNING: Basic block too many instructions: " + 
                decstr(icount) + "\n");
            assert(false);
        }

        // Add instrumentation call to check if buffer needs to be flushed.
        BBL_InsertIfCall(bbl, IPOINT_BEFORE,
                         (AFUNPTR) CheckBuffer,
                         IARG_UINT32, icount,
                         IARG_END);

        BBL_InsertThenCall(bbl, IPOINT_BEFORE,
                           (AFUNPTR) FlushBuffer,
                           IARG_BOOL, true,
                           IARG_CONTEXT,
                           IARG_THREAD_ID,
                           IARG_BOOL, true,
                           IARG_END);

    }

    if (g_logCoverage || g_bbfile)
    {
    // Add instrumentation call to log block hits
    BBL_InsertCall(bbl, IPOINT_BEFORE,
      AFUNPTR(LogBlockHit), IARG_FAST_ANALYSIS_CALL,
      IARG_THREAD_ID,
      IARG_INST_PTR,
      IARG_END);
    }
    //LOG("INS: BBL ins start.\n");

    // Count of instructions that have yet to be inserted into the buffer,
    // at the point at which the current instruction will be executed.
    uint32_t insLeft = icount;

    for (INS ins = BBL_InsHead(bbl); INS_Valid(ins); ins = INS_Next(ins)) {
        if (!g_active && g_usetrigger) {
            // Logging has not been activated yet, so all we need to do now
            // is check for the trigger condition.

            if (INS_Address(ins) == g_trig_addr) {
                // Found the trigger address.

                INS_InsertIfCall(ins, IPOINT_BEFORE,
                                 (AFUNPTR) CheckTrigger,
                                 IARG_END);

                INS_InsertThenCall(ins, IPOINT_BEFORE,
                                   (AFUNPTR) Activate,
                                   IARG_CONTEXT,
                                   IARG_END);

            }

            // Skip the rest of the analysis and immediately go on to the
            // next instruction.
            continue;

        }

        // Skip instrumentation unless g_active is enabled
        if (!g_active) {
            continue;
        }

        // Add instrumentation call to insert instruction into buffer.

        if (INS_Category(ins) == XED_CATEGORY_X87_ALU) {

            // TODO: Handle floating point instructions.
            // LOG("Not logging FP instruction.\n");

            // cerr << "Not logging FP instruction @" << INS_Address(ins) << ": " << INS_Disassemble(ins) << endl;
            // continue;

        } else if (INS_Category(ins) == XED_CATEGORY_PREFETCH) {
            LOG("Not logging prefetch instruction.\n");
            cerr << "Not logging prefetch instruction @" << INS_Address(ins) << ": " << INS_Disassemble(ins) << endl;
            continue;
        } else if (INS_Category(ins) == XED_CATEGORY_MMX) {
            LOG("Not logging mmx instruction.\n");
            cerr << "Not logging mmx instruction @" << INS_Address(ins) << ": " << INS_Disassemble(ins) << endl;
            continue;
        } else if (INS_Category(ins) == XED_CATEGORY_FCMOV) {
            LOG("Not logging float move instruction.\n");
            cerr << "Not logging float move instruction @" << INS_Address(ins) << ": " << INS_Disassemble(ins) << endl;
            continue;
        }

        // Check if there's a REP prefix.
        if (INS_HasRealRep(ins)) {

            INS_InsertIfCall(ins, IPOINT_BEFORE,
                             (AFUNPTR) CheckBufferEx,
                             IARG_FIRST_REP_ITERATION,
                             IARG_REG_VALUE, INS_RepCountRegister(ins),
                             IARG_UINT32, insLeft,
                             IARG_END);

            INS_InsertThenCall(ins, IPOINT_BEFORE,
                               (AFUNPTR) FlushBuffer,
                               IARG_BOOL, false,
                               IARG_ADDRINT, 0,
                               IARG_THREAD_ID,
                               IARG_BOOL, true,
                               IARG_END);

        }

        // The argument list to be passed into the instruction analysis call.
        IARGLIST arglist = IARGLIST_Alloc();
        IARGLIST arglist_helper = IARGLIST_Alloc();
        valcount = 0;
      
        // The first few arguments to AppendBuffer.
        IARGLIST_AddArguments(arglist,
                              IARG_ADDRINT, INS_Address(ins),
                              IARG_THREAD_ID,
                              IARG_CONTEXT,
                              IARG_BOOL, INS_IsBranch(ins),
                              IARG_UINT32, INS_Size(ins),
                              IARG_END);

        // Now we need to gather the instruction bytes.

        // Wastes a copy, but required because the instruction may not be
        // 32-bit aligned, and we want to respect word alignment requirements.
        uint32_t rawbytes_i[4];
        // Is it an xor?
        bool is_xor = false;

        UINT sz = INS_Size(ins);
        assert(PIN_SafeCopy((void*)rawbytes_i, (const void*) INS_Address(ins), sz) == sz);
      
        IARGLIST_AddArguments(arglist,
                              IARG_UINT32, rawbytes_i[0],
                              IARG_UINT32, rawbytes_i[1],
                              IARG_UINT32, rawbytes_i[2],
                              IARG_UINT32, rawbytes_i[3],
                              IARG_END);

        for (uint32_t i = 0; i < MAX_VALUES_COUNT; i++) {
            opndvals[i].taint = 0;
            opndvals[i].type = INVALIDREGMEM;
        }

        // specializing xors
        if (INS_Mnemonic(ins) == string("XOR") ||
            INS_Mnemonic(ins) == string("PXOR")) {
            int opnum = -1;
            bool found = false;
            REG r = LEVEL_BASE::REG_INVALID();

            /* Find the source and destination operand. */
            for (uint32_t i = 0 ; i < INS_OperandCount(ins); i++) {
                if (INS_OperandReadAndWritten (ins, i) &&
                    INS_OperandIsReg(ins, i)) {
                } else {
                    found = true;
                    r = INS_OperandReg(ins, i);
                    opnum = -1;
                    break;
                }
            }

            /* Find the second operand, and ensure it's the same register
               as the first operand we found. */
            if (found) {
                for (uint32_t i = 0 ; i < INS_OperandCount(ins); i++) {
                    if (INS_OperandReadAndWritten (ins, i) &&
                        INS_OperandIsReg(ins, i) &&
                        r == INS_OperandReg(ins, i) &&
                        (unsigned)opnum != i) {
                        is_xor = true;
                        break;
                    }
                }
            }
        } /* end xor code */

        for(uint32_t i = 0; i < INS_OperandCount(ins); i++) {

            opndvals[valcount].taint = 0;
            if (INS_OperandRead(ins, i) && (!is_xor))
                opndvals[valcount].taint = RD;
            if (INS_OperandWritten(ins, i))
                opndvals[valcount].taint |= WR;
	
            /* Handle register operands */
            if (INS_OperandIsReg(ins, i)) {
         
                REG r = INS_OperandReg(ins, i);
                if(r == LEVEL_BASE::REG_INVALID()) {
                  cerr << "Warning: invalid register operand in " << INS_Disassemble(ins) << endl;
                  continue;
                }
                assert(r != LEVEL_BASE::REG_INVALID());
                opndvals[valcount].reg = r;
                opndvals[valcount].type.type = REGISTER;

                // This was causing problems with movd %eax, %xmm0,
                // because %xmm0's operand width is 32, but BAP needs
                // to know the full operand size, which is 128.
                // opndvals[valcount].type.size = INS_OperandWidth(ins, i);

                opndvals[valcount].type.size = GetBitsOfReg(r);

                REG fullr = REG_FullRegName(r);
                if (fullr != REG_INVALID() && fullr != r) {
                  /* We know the fuller register, so just use that! */
                    //	      cerr << "partial " << REG_StringShort(r) << " full " << REG_StringShort(fullr) << endl;
                    opndvals[valcount].reg = fullr;
                    opndvals[valcount].type.type = REGISTER;
                    opndvals[valcount].type.size = GetBitsOfReg(fullr);
                }

                valcount++;

            } else if (INS_OperandIsMemory(ins, i) ||
                       INS_OperandIsAddressGenerator(ins, i)) {


                /* Note: Compiled code sometimes uses LEA instructions for
                 * arithmetic.  As such, we always want to treat reads of
                 * these base/index registers as tainted. */

                REG basereg = INS_OperandMemoryBaseReg(ins, i);
                if (basereg != LEVEL_BASE::REG_INVALID()) {

                    opndvals[valcount].reg = basereg;
                    opndvals[valcount].type.type = REGISTER;
                    opndvals[valcount].type.size = GetBitsOfReg(basereg);

                    if (TaintedIndices || INS_OperandIsAddressGenerator(ins, i))
                        opndvals[valcount].taint = RD;
                    else
                        opndvals[valcount].taint = 0;

                    valcount++;

                }

                REG idxreg = INS_OperandMemoryIndexReg(ins, i);
                if (idxreg != LEVEL_BASE::REG_INVALID()) {

                    opndvals[valcount].reg = idxreg;
                    opndvals[valcount].type.type = REGISTER;
                    opndvals[valcount].type.size = GetBitsOfReg(idxreg);

                    if (TaintedIndices || INS_OperandIsAddressGenerator(ins, i))
                        opndvals[valcount].taint = RD;
                    else
                        opndvals[valcount].taint = 0;

                    valcount++;              

                }
            } 	   
        }

        bool memRead = INS_IsMemoryRead(ins);
        bool memRead2 = INS_HasMemoryRead2(ins);
        bool memWrite = INS_IsMemoryWrite(ins);

        // Value type of memory read.
        RegMem_t memReadTy = {NONE , 0};
        if (memRead || memRead2) {
            memReadTy.size = (INS_MemoryReadSize(ins) * 8); 
            memReadTy.type = MEM;
        }
        // Value type of memory write
        RegMem_t memWriteTy = {NONE , 0};
        if (memWrite) {
            memWriteTy.size = (INS_MemoryWriteSize(ins) * 8); 
            memWriteTy.type = MEM;
        }
         
        // Insert the operand values we've previously identified into the arglist.
        for (unsigned int i = 0; i < valcount; i++) {

            // cerr << opndvals[i].type << " " << i << " " << valcount << endl;
        
            // LOG("Adding: " + REG_StringShort((REG)opndvals[i].reg) + "\n");

            /*
             * PIN has several ways of passing register values to analysis
             * functions.  Unfortunately, none of them works all the
             * time.  So, we need to decide how to pass the value, and set
             * the *_value arguments to AppendBuffer accordingly.
             */
            switch (howPass((REG) opndvals[i].reg)) {
            case P_FPX87:
            case P_CONTEXT:
                IARGLIST_AddArguments(arglist_helper,
                                      IARG_UINT32, (uint32_t)(opndvals[i].type.type),
                                      IARG_UINT32, opndvals[i].type.size,
                                      IARG_UINT32, opndvals[i].reg,
                                      /* We don't need the value
                                         argument for contexts */
                                      IARG_PTR, 0,
                                      IARG_UINT32, opndvals[i].taint,
                                      IARG_END);        
                break;

            case P_REF:
                IARGLIST_AddArguments(arglist_helper, 
                                      IARG_UINT32, (uint32_t)(opndvals[i].type.type),
                                      IARG_UINT32, opndvals[i].type.size,
                                      IARG_UINT32, opndvals[i].reg,
                                      /* Pass reference pointer */
                                      IARG_REG_CONST_REFERENCE, opndvals[i].reg,
                                      IARG_UINT32, opndvals[i].taint,
                                      IARG_END);        
                break;
              
            default:
                cerr << "Unknown value passing method" << endl;
                assert(false);
            }
        }

        /* We break up memory operands into byte-wise operands.  This is
         * essential for taint analysis.  Code that utilizes taint
         * analysis assumes that a tainted value can be computed (e.g.,
         * symbolically executed) using the instructions in the trace.
         * However, if some of a memory operand are not tainted, then
         * they could have changed.  Thus, we must break up memory
         * operands to make this explicit. */
      
        if (memRead) {
            uint32_t bytes = GetByteSize(memReadTy);
        
            for (uint32_t offset = 0; offset < bytes; offset++) {
                IARGLIST_AddArguments(arglist_helper,
                                      IARG_UINT32, (uint32_t)MEM,
                                      IARG_UINT32, 8, // one byte
                                      IARG_MEMORYREAD_EA,
                                      //IARG_MEMORYREAD_SIZE,
                                      IARG_UINT32, offset,
                                      IARG_UINT32, RD,
                                      IARG_END);
                valcount++;
            }
        }

        if (memRead2) {
            uint32_t bytes = GetByteSize(memReadTy);

            for (uint32_t offset = 0; offset < bytes; offset++) {        
                IARGLIST_AddArguments(arglist_helper,
                                      IARG_UINT32, (uint32_t)MEM,
                                      IARG_UINT32, 8, // one byte
                                      IARG_MEMORYREAD2_EA,
                                      //IARG_MEMORYREAD_SIZE,
                                      IARG_UINT32, offset,
                                      IARG_UINT32, RD,
                                      IARG_END);
                valcount++;
            }
        }

        if (memWrite) {
            uint32_t bytes = GetByteSize(memWriteTy);

            for (uint32_t offset = 0; offset < bytes; offset++) {        
          
                IARGLIST_AddArguments(arglist_helper,
                                      IARG_UINT32, (uint32_t)MEM,
                                      IARG_UINT32, 8, // one byte
                                      IARG_MEMORYWRITE_EA,
                                      //IARG_MEMORYWRITE_SIZE,
                                      IARG_UINT32, offset,
                                      IARG_UINT32, WR,
                                      IARG_END);
                valcount++;
            }
        }

        if (INS_SegmentPrefix(ins)) {
            REG seg = INS_SegmentRegPrefix(ins);
            /* Pin only has base registers for FS and GS (probably since
               Linux uses GS, and Windows uses FS. So, we'll just output a
               base register if we see one of those for now, and hope we
               don't need ES/etc. */
            if (seg == LEVEL_BASE::REG_SEG_FS || seg == REG_SEG_GS) {
                REG addreg;

                /* Set the register to add to the buffer */
                switch(seg) {
                case LEVEL_BASE::REG_SEG_FS:
                    addreg = LEVEL_BASE::REG_SEG_FS_BASE;
                    break;
	    
                case LEVEL_BASE::REG_SEG_GS:
                    addreg = LEVEL_BASE::REG_SEG_GS_BASE;
                    break;

                default:
                    assert(false);
                    break;
                }

                IARGLIST_AddArguments(arglist_helper,
                                      IARG_UINT32, (uint32_t)REGISTER,
                                      IARG_UINT32, 32, // Register size in bits
                                      IARG_UINT32, addreg,
                                      //IARG_MEMORYWRITE_SIZE,
                                      IARG_PTR, 0,
                                      IARG_UINT32, 0,
                                      IARG_END);	  
                valcount++;
            }
        }



        // TODO: Check if valcount has exceed the maximum number of
        // values. Also, figure out what to do if so.

        if (valcount >= MAX_VALUES_COUNT) {
            cerr << "Error: Too many values (" << valcount << "). Max: " << MAX_VALUES_COUNT << endl;
            cerr << "Instruction: " << INS_Disassemble(ins) << endl;
            cerr << "Category: " << CATEGORY_StringShort(INS_Category(ins)) << endl;
        }
        assert(valcount < MAX_VALUES_COUNT);
      

        IARGLIST_AddArguments(arglist,
                              IARG_UINT32, valcount,
                              IARG_END);

        /* Now, add the operands. */
        IARGLIST_AddArguments(arglist,
                              IARG_IARGLIST, arglist_helper,
                              IARG_END);

        // The argument list has been built, time to insert the call.

        INS_InsertCall(ins, IPOINT_BEFORE,
                       (AFUNPTR) AppendBuffer,
                       IARG_IARGLIST, arglist,
                       IARG_END);

        // If we are logging one instruction after exiting the recording
        // range, then arrange for the post instruction call to happen
        // if ins is outside of the range.
        if (LogOneAfter.Value() && !(INS_Address(ins) >= start_addr && INS_Address(ins) <= end_addr)) {
            cerr << "found the last one" << endl;
            if (INS_IsBranchOrCall(ins)) {
                INS_InsertCall(ins, IPOINT_TAKEN_BRANCH,
                               (AFUNPTR) PostInstruction,
                               IARG_ADDRINT, INS_Address(ins),
                               IARG_CONTEXT,
                               IARG_END);
            } else {
                INS_InsertCall(ins, IPOINT_AFTER,
                               (AFUNPTR) PostInstruction,
                               IARG_ADDRINT, INS_Address(ins),
                               IARG_CONTEXT,
                               IARG_END);
            }
        }
      
        insLeft--;

        // Free the memory.
        IARGLIST_Free(arglist);
        IARGLIST_Free(arglist_helper);

    }

    //LOG("INS: bbl ins end.\nINS: BBL end.\n");
   

}

/// Utility functions
// Walk the modules_blacklisted list and check if address belongs to one of the 
// blacklisted module
bool is_address_blacklisted(ADDRINT address)
{
    for(MODULE_BLACKLIST_T::const_iterator it = modules_blacklisted.begin(); 
        it != modules_blacklisted.end(); ++it)
    {
        ADDRINT low_address = it->second.first;
        ADDRINT high_address = it->second.second;
        if(address >= low_address && address <= high_address)
            return true;
    }

    return false;
}

// Check is image_path matches one of the string in the blacklist
bool is_module_blacklisted(const std::string &image_path)
{
    // If the path of a DLL matches one of the following string, the module 
    // won't be instrumented by Pin.
    // This way you can avoid instrumentation of Windows API.
    static char* black[] = {
      (char*)"C:\\Windows\\",
      (char*)"/lib", (char*)"/usr/lib"
    };

    for(unsigned int i = 0; i < sizeof(black) / sizeof(black[0]); ++i)
        if(strncasecmp(black[i], image_path.c_str(), strlen(black[i])) == 0)
            return true;

    return false;
}

VOID InstrTrace(TRACE trace, VOID *v)
{
    if(g_bbfile){
      // We don't want to instrument BBs contained in the Windows/unix libs
      if(is_address_blacklisted(TRACE_Address(trace))){
        return;
      }
    }

    /* Decide if we want to log this trace by examining the entrance address. */
    ADDRINT addr = TRACE_Address(trace);
    if (dontLog(addr)) {
      return;
    }

    ADDRINT bb_addr;
    set<ADDRINT>::iterator it;
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl)) {
      bb_addr = BBL_Address(bbl);
      
      // Skip BBs visited in previous runs
      it = g_visited_bbs.find(bb_addr);
      if(it != g_visited_bbs.end()){
        continue;
      }
      InstrBlock(bbl);
    }
}

VOID ThreadEnd(THREADID threadid, CONTEXT *ctx, INT32 code, VOID *v)
{
    ThreadInfo_t *ti = NULL;
  
    cerr << "Thread " << threadid << " ending" << endl;

    // Free thread-local data
    ti = GetThreadInfo();
  
    delete ti;
}

VOID ThreadStart(THREADID threadid, CONTEXT *ctx, INT32 flags, VOID *v)
{
    // Get the command line arguments before _start is called
    // This only works with Linux conventions in mind
    static int firstthread = true;

    LLOG("new thread\n");
  
    NewThreadInfo();

    GetLock(&lock, threadid+1);
  
    LOG("New thread starting\n");
    cerr << "Thread " << threadid << " starting" << endl;

    if (firstthread) {
        firstthread = false;
#ifndef _WIN32 /* unix */
        int argc = *(int*)(PIN_GetContextReg(ctx, LEVEL_BASE::REG_ESP));
        char **argv = (char**) (PIN_GetContextReg(ctx, LEVEL_BASE::REG_ESP)+4);
        char **env = (char**) (PIN_GetContextReg(ctx, LEVEL_BASE::REG_ESP)+(argc+1)*4);
        std::vector<frame> frms = tracker->taintArgs(argc, argv);
        g_twnew->add<std::vector<frame> > (frms);
        frms = tracker->taintEnv(env);
        g_twnew->add <std::vector<frame> > (frms);
#else /* windows */
        /* On windows, we don't taint argc and argv, but rather taint the
           output of GetComamndLineA and GetCommandLineW.  On recent
           versions of Windows, these return a static pointer. */
        char *aptr = WINDOWS::GetCommandLineA();
        wchar_t *wptr = WINDOWS::GetCommandLineW();

        std::vector<frame> frms = tracker->taintArgs(aptr, wptr);
        g_twnew->add<std::vector<frame> > (frms);
#endif
    }

    ReleaseLock(&lock);  

}

// If module is blacklisted add its address range to address blacklist.
VOID fill_blacklist(IMG img)
{
    const std::string image_path = IMG_Name(img);
    //Bail if module isn't on blacklist
    if(!is_module_blacklisted(image_path)){
      return;
    }

    ADDRINT module_low_limit = IMG_LowAddress(img);
    ADDRINT module_high_limit = IMG_HighAddress(img); 

    std::pair<std::string, std::pair<ADDRINT, ADDRINT> > module_info = 
      std::make_pair(
        image_path,
        std::make_pair(
            module_low_limit,
            module_high_limit
        )
    );

    //module_list.insert(module_info);
    modules_blacklisted.insert(module_info);
}

VOID ModLoad(IMG img, VOID *v)
{
    cerr << "This is modload()" << endl;
    
    fill_blacklist(img);

    const string &name = IMG_Name(img);
  
    frame f;
    f.mutable_modload_frame()->set_module_name(name);
    f.mutable_modload_frame()->set_low_address(IMG_LowAddress(img));
    f.mutable_modload_frame()->set_high_address(IMG_HighAddress(img));

    g_twnew->add(f);

#ifdef _WIN32
    // Try to find kernel32
    {
        char tempbuf[BUFSIZE];
        char *tok = NULL;
        char *lasttok = NULL;

        // Fill up the temporary buffer
        strncpy(tempbuf, name.c_str(), BUFSIZE);

        // We don't need a lock, since this is an instrumentation function (strtok is not re-entrant)
        strtok(tempbuf, "\\");

        while ((tok = strtok(NULL, "\\")) != NULL) {
            // Just keep parsing...
            lasttok = tok;
        }

        if (lasttok) {
            if (strncmp(windowsDll, lasttok, BUFSIZE) == 0) {

                /* GetEnvironmentStringsA uses a table-based conversion
                 * process that we can't analyze very well. So, these are
                 * disabled. */
#ifdef USE_GETENVSTRINGS
                RTN r;
	       
                /** The prototype for GetEnvironmentStrings[WA] */
                PROTO proto = PROTO_Allocate( PIN_PARG(uint32_t), CALLINGSTD_STDCALL,
                                              "Windows API",
                                              PIN_PARG_END() );
	       
                r = RTN_FindByName(img, "GetEnvironmentStringsW");
                if (r != RTN_Invalid()) {				    
                    RTN_ReplaceSignature(r, AFUNPTR(GetEnvWWrap),
                                         IARG_PROTOTYPE, proto,
                                         IARG_CONTEXT,
                                         IARG_ORIG_FUNCPTR,
                                         IARG_THREAD_ID,
                                         IARG_END);
                } else {
                    cerr << "Warning: Error instrumenting GetEnvironmentStringsW()" << endl;
                }

                r = RTN_FindByName(img, "GetEnvironmentStringsA");
                if (r != RTN_Invalid()) {				    
                    RTN_ReplaceSignature(r, AFUNPTR(GetEnvAWrap),
                                         IARG_PROTOTYPE, proto,
                                         IARG_CONTEXT,
                                         IARG_ORIG_FUNCPTR,
                                         IARG_THREAD_ID,
                                         IARG_END);
                } else {
                    cerr << "Warning: Error instrumenting GetEnvironmentStringsA()" << endl;
                }

	       
                PROTO_Free(proto);
#endif
	       
            } else if (strncmp(wsDll, lasttok, BUFSIZE) == 0) {
                /* Winsock */
                RTN r;

                cerr << "found winsock" << endl;

                r = RTN_FindByName(img, "accept");
                if (r != RTN_Invalid()) {

                    PROTO proto = PROTO_Allocate(PIN_PARG(uint32_t), CALLINGSTD_STDCALL,
                                                 "accept",
                                                 PIN_PARG(uint32_t),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(int*),
                                                 PIN_PARG_END());

                    RTN_ReplaceSignature(r, AFUNPTR(AcceptWrapper),
                                         IARG_PROTOTYPE, proto,
                                         IARG_CONTEXT,
                                         IARG_ORIG_FUNCPTR,
                                         IARG_THREAD_ID,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                                         IARG_END);


                    PROTO_Free(proto);
					
                } else {
                    cerr << "Couldn't find accept" << endl;
                }

                r = RTN_FindByName(img, "connect");
                if (r != RTN_Invalid()) {

                    PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
                                                 "connect",
                                                 PIN_PARG(uint32_t),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG_END());

                    RTN_ReplaceSignature(r, AFUNPTR(ConnectWrapper),
                                         IARG_PROTOTYPE, proto,
                                         IARG_CONTEXT,
                                         IARG_ORIG_FUNCPTR,
                                         IARG_THREAD_ID,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                                         IARG_END);


                    PROTO_Free(proto);
					
                } else {
                    cerr << "Couldn't find connect" << endl;
                }

                r = RTN_FindByName(img, "WSAConnect");
                if (r != RTN_Invalid()) {

                    PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
                                                 "WSAConnect",
                                                 PIN_PARG(uint32_t),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG_END());

                    RTN_ReplaceSignature(r, AFUNPTR(WSAConnectWrapper),
                                         IARG_PROTOTYPE, proto,
                                         IARG_CONTEXT,
                                         IARG_ORIG_FUNCPTR,
                                         IARG_THREAD_ID,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 3,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 4,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 5,
                                         IARG_FUNCARG_ENTRYPOINT_VALUE, 6,
                                         IARG_END);


                    PROTO_Free(proto);
					
                } else {
                    cerr << "Couldn't find WSAConnect" << endl;
                }


                r = RTN_FindByName(img, "recv");
                if (r != RTN_Invalid()) {

                    PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
                                                 "recv",
                                                 PIN_PARG(uint32_t),
                                                 PIN_PARG(char*),
                                                 PIN_PARG(int),
                                                 PIN_PARG(int),
                                                 PIN_PARG_END());
                    RTN_Open(r);

                    RTN_InsertCall(r, IPOINT_BEFORE, (AFUNPTR)BeforeRecv,
                                   IARG_PROTOTYPE, proto,
                                   IARG_THREAD_ID,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                                   IARG_END);

                    RTN_InsertCall(r, IPOINT_AFTER, (AFUNPTR)AfterRecv,
                                   IARG_PROTOTYPE, proto,
                                   IARG_THREAD_ID,
                                   IARG_FUNCRET_EXITPOINT_VALUE,
                                   IARG_PTR, "recv",
                                   IARG_END);

                    RTN_Close(r);
                    PROTO_Free(proto);

                } else {
                    cerr << "Couldn't find recv" << endl;
                }


                r = RTN_FindByName(img, "recvfrom");
                if (r != RTN_Invalid()) {

                    PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
                                                 "recvfrom",
                                                 PIN_PARG(uint32_t),
                                                 PIN_PARG(char*),
                                                 PIN_PARG(int),
                                                 PIN_PARG(int),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(int*),
                                                 PIN_PARG_END());
#if 0
                    RTN_Open(r);

                    RTN_InsertCall(r, IPOINT_BEFORE, (AFUNPTR)BeforeRecv,
                                   IARG_PROTOTYPE, proto,
                                   IARG_THREAD_ID,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                                   IARG_END);

                    RTN_InsertCall(r, IPOINT_AFTER, (AFUNPTR)AfterRecv,
                                   IARG_PROTOTYPE, proto,
                                   IARG_THREAD_ID,
                                   IARG_FUNCRET_EXITPOINT_VALUE,
                                   IARG_PTR, "recvfrom",
                                   IARG_END);

                    RTN_Close(r);
#endif
                    PROTO_Free(proto);

                } else {
                    cerr << "Couldn't find recvfrom" << endl;
                }         
         
                r = RTN_FindByName(img, "WSARecv");
                if (r != RTN_Invalid()) {

                    PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
                                                 "WSARecv",
                                                 PIN_PARG(uint32_t),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(uint32_t),
                                                 PIN_PARG(uint32_t*),
                                                 PIN_PARG(uint32_t*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG_END());

                    RTN_Open(r);

                    RTN_InsertCall(r, IPOINT_BEFORE, (AFUNPTR)WSABeforeRecv,
                                   IARG_PROTOTYPE, proto,
                                   IARG_THREAD_ID,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 3,
                                   IARG_END);

                    RTN_InsertCall(r, IPOINT_AFTER, (AFUNPTR)AfterRecv,
                                   IARG_PROTOTYPE, proto,
                                   IARG_THREAD_ID,
                                   IARG_FUNCRET_EXITPOINT_VALUE,
                                   IARG_PTR, "WSARecv",
                                   IARG_END);

                    RTN_Close(r);
                    PROTO_Free(proto);

                } else {
                    cerr << "Couldn't find WSArecv" << endl;
                }

                r = RTN_FindByName(img, "WSARecvFrom");
                if (r != RTN_Invalid()) {

                    PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
                                                 "WSARecvFrom",
                                                 PIN_PARG(uint32_t),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG(void*),
                                                 PIN_PARG_END());

#if 0
                    RTN_Open(r);

                    RTN_InsertCall(r, IPOINT_BEFORE, (AFUNPTR)WSABeforeRecv,
                                   IARG_PROTOTYPE, proto,
                                   IARG_THREAD_ID,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                                   IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                                   IARG_END);

                    RTN_InsertCall(r, IPOINT_AFTER, (AFUNPTR)AfterRecv,
                                   IARG_PROTOTYPE, proto,
                                   IARG_THREAD_ID,
                                   IARG_FUNCRET_EXITPOINT_VALUE,
                                   IARG_PTR, "WSARecvFrom",
                                   IARG_END);

                    RTN_Close(r);
#endif

                    PROTO_Free(proto);

                } else {
                    cerr << "Couldn't find WSArecv" << endl;
                }

            } else {
                cerr << "Other img: " << lasttok << endl;
            }
        }
    }
#endif

    if (g_usetrigger && !g_trig_resolved) {
        // Check if this module can be used to resolve the trigger address.

        // If no trigger module is set, then we just use the first one we
        // find, i.e. the main module.
        if ((KnobTrigModule.Value() == "") ||
            name.find(KnobTrigModule.Value()) != string::npos) {
            // Found the module, resolve address.

            g_trig_addr = KnobTrigAddr.Value() + IMG_LoadOffset(img);
            g_trig_resolved = true;

        }

    }
   
}

VOID SyscallEntry(THREADID tid, CONTEXT *ctx, SYSCALL_STANDARD std, VOID *v)
{
    ThreadInfo_t *ti = NULL;
    SyscallInfo_t si;

    /*
     * Synchronization note: We assume there is only one system call per
     * thread, and thus the thread local syscall stack does not need any
     * locking.
     */
  
    // cerr << "syscall in " << PIN_GetSyscallNumber(ctx, std) << endl;

    ti = GetThreadInfo();
    //  cerr << "stack size " << ti->scStack.size() << endl;

    LLOG("syscall\n");

    // Ignore if not activated.
    //  if (!g_active) return;

    // Get the address from instruction pointer (should be EIP).
    si.sf.mutable_syscall_frame()->set_address(PIN_GetContextReg(ctx, LEVEL_BASE::REG_INST_PTR));

    si.sf.mutable_syscall_frame()->set_thread_id(tid);

    si.sf.mutable_syscall_frame()->set_number(PIN_GetSyscallNumber(ctx, std));

    for (int i = 0; i < MAX_SYSCALL_ARGS; i++)
        {
            if (i < PLAT_SYSCALL_ARGS) {
                si.sf.mutable_syscall_frame()->mutable_argument_list()->add_elem(PIN_GetSyscallArgument(ctx, std, i));
            }
        }

    // XXX: This should really be above g_active probably, but it seems
    // unlikely that it would cause a problem.  It's here because
    // FlushBuffer obtains a lock of it's own.
    GetLock(&lock, tid+1);

    // First we need to flush the buffer, so we can directly add the
    // syscall frame after the frame for the instruction that led to the
    // syscall.
    FlushBuffer(true, ctx, tid, false);

    if (LogAllSyscalls.Value()) {
        g_twnew->add(si.sf);
    }
  
    if (tracker->taintPreSC(si.sf.mutable_syscall_frame()->number(), (const uint64_t *) (si.sf.syscall_frame().argument_list().elem().data()), si.state)) {
        // Do we need to do anything here? ...
    }
  
    ti->scStack.push(si);
  
    //e:

    LLOG("releasing sysenter\n");
    ReleaseLock(&lock);  
    LLOG("really done with sysenter\n");
}

VOID SyscallExit(THREADID tid, CONTEXT *ctx, SYSCALL_STANDARD std, VOID *v)
{
    ThreadInfo_t *ti = NULL;
    SyscallInfo_t si;
    uint32_t addr, length;

    /*
     * Synchronization note: We assume there is only one system call per
     * thread, and thus the thread local syscall stack does not need any
     * locking.
     */

    // Ignore if not activated.
    // if (!g_active) return;

    LLOG("sysexit\n");
 
    ti = GetThreadInfo();
 
    si = ti->scStack.top();
    ti->scStack.pop();

    GetLock(&lock, tid+1);
  
    // Check to see if we need to introduce tainted bytes as a result of this
    // sytem call
    FrameOption_t fo = tracker->taintPostSC(PIN_GetSyscallReturn(ctx, std), (const uint64_t*) (si.sf.syscall_frame().argument_list().elem().data()), addr, length, si.state);

    if (fo.b) {
        if (!g_taint_introduced) {
            // Activate taint tracking
            TActivate();
        }
        g_twnew->add(fo.f);
    }

    //printf("syscall out %d\n", si.sf.callno);

    LLOG("releasing sysexit\n");
    ReleaseLock(&lock);  

    // Untaint system call output registers (uses thread-local delta, so no lock needed)
    tracker->postSysCall(ti->delta);

    LLOG("really done with sysexit\n");
}


VOID FollowParent(THREADID threadid, const CONTEXT* ctxt, VOID * arg)
{
    int i;

    LLOG("fparent\n");
  
    GetLock(&lock, threadid+1);
    i = strlen(g_threadname);
    assert(i < BUFFER_SIZE);
    g_threadname[i++] = 'p';

    std::cerr << "Spawning parent: " << PIN_GetPid() << g_threadname << std::endl;

    ReleaseLock(&lock);  
}
  
VOID ExceptionHandler(THREADID threadid, CONTEXT_CHANGE_REASON reason, const CONTEXT *from, CONTEXT *to, INT32 info, VOID *v) {
    /*
      CONTEXT_CHANGE_REASON_FATALSIGNAL 	 Receipt of fatal Unix signal.
      CONTEXT_CHANGE_REASON_SIGNAL 	 Receipt of handled Unix signal.
      CONTEXT_CHANGE_REASON_SIGRETURN 	 Return from Unix signal handler.
      CONTEXT_CHANGE_REASON_APC 	 Receipt of Windows APC.
      CONTEXT_CHANGE_REASON_EXCEPTION 	 Receipt of Windows exception.
      CONTEXT_CHANGE_REASON_CALLBACK 	 Receipt of Windows call-back.
    */

    /*
      If there is a fatal exception, we should halt the trace as soon
      as possible, so we can exit.
    
      Also, FlushInstructions() needs mutual exclusivity.
    */

    // SWHITMANXXX Get information and put it into exception frame here
    // XXX Put this into frame buffer

    frame f;
    f.mutable_exception_frame()->set_exception_number(info);
    f.mutable_exception_frame()->set_thread_id(threadid);
    if (from) {
        f.mutable_exception_frame()->set_from_addr(PIN_GetContextReg(from, LEVEL_BASE::REG_INST_PTR));
    }
    if (to) {
        f.mutable_exception_frame()->set_to_addr(PIN_GetContextReg(to, LEVEL_BASE::REG_INST_PTR));
    }

    GetLock(&lock, threadid+1);  
    LLOG("got except lock!\n");

    //Dump it to the specified file
    if(KnobExnFile.Value() != ""){
      ofstream exf(KnobExnFile.Value().c_str());
      if (exf.is_open())
      {
        ADDRINT pc = PIN_GetContextReg(from, LEVEL_BASE::REG_INST_PTR);
        exf << "Exception " << info << " at 0x" << std::hex << pc << endl;
        exf.close();
      }
      else{
        cerr << "Can't open " << KnobExnFile.Value();
        assert(FALSE);
      }
    }

    // If we want the exception to be the last thing in the trace when
    // we crash, then we need to flush.
    FlushBuffer(false, from, threadid, false);
    g_twnew->add(f);
    
    if(g_stack_dump>0){
      AppendStackDumping(threadid, from);
    }
    if(g_mem_snapshot>0){
      ThreadInfo_t *ti = GetThreadInfo();
      TakeSnapshot(from, tracker, ti->delta, SnapshotFile.Value().c_str());
    }

    if (reason == CONTEXT_CHANGE_REASON_FATALSIGNAL) {
        std::cerr << "Received fatal signal " << info << endl;
        FlushBuffer(false, from, threadid, false);
        Cleanup();
        exit(0);
    } else if (reason == CONTEXT_CHANGE_REASON_EXCEPTION) {

#ifdef _WIN32
        ADDRINT pc = PIN_GetContextReg(from, LEVEL_BASE::REG_INST_PTR);
        cerr << "Received windows exception @" << pc << " " << info << " in thread " << threadid << endl;

        if (info == accessViolation && SEHMode.Value() && g_taint_introduced) {
            cerr << "SEH mode activated!" << endl;
            ADDRINT old_esp = PIN_GetContextReg(from, LEVEL_BASE::REG_STACK_PTR);
            ADDRINT new_esp = PIN_GetContextReg(to, LEVEL_BASE::REG_STACK_PTR);

            cerr << "old esp: " << old_esp << " new esp: " << new_esp
                 << " old eip: " << PIN_GetContextReg(from, LEVEL_BASE::REG_INST_PTR) << " new eip: " << PIN_GetContextReg(to, REG_INST_PTR) << endl;
      
            /* The windows exception handler just pushed a bunch of crap
               onto the stack.  Although some of this is user controllable,
               we can untaint it for now, since we are mainly concerned with
               accessing our buffer. */
            assert (new_esp < old_esp);
            for (ADDRINT ptr = new_esp; ptr < old_esp; ptr++) {
                tracker->untaintMem(ptr);
            }

            /* Try to find a tainted exception handler. */
            ADDRINT eptr = PIN_GetContextReg(to, LEVEL_BASE::REG_SEG_FS_BASE) 
                + ehandler_fs_offset;

            /* eptr points to the &(head of SEH). */
            assert(PIN_SafeCopy(&eptr, (void*)eptr, sizeof(ADDRINT)) == sizeof(ADDRINT));

            /* eptr points to head of SEH. */

            for (int i = 0; i < maxSehLength; i++) {
                // while (true) {
                struct {
                    ADDRINT nptr;
                    ADDRINT handler;
                } buf;
                /* We supposedly have a pointer to an exception handler
                   structure.  Let's make sure it's mapped. */
                size_t b = PIN_SafeCopy((void*)&buf, (void*)eptr, ehandler_size);
                if (b == ehandler_size) {

                    /* Okay, we have an exception handler.  Let's see if the
                       pointer handler is tainted.  So, check if M[eptr+4] is
                       tainted. */
                    ADDRINT hptr = eptr + ehandler_handler_offset;

                    /* hptr holds the address of the handler. */
                    cerr << "SEH handler M[" << hptr 
                         << "] = " << buf.handler
                         << " (" << tracker->getMemTaint(hptr, pintrace::INVALIDREGMEM) 
                         << ")"
                         << endl;

                    eptr = buf.nptr;
	
                } else { 
                    cerr << "Unable to read from " << eptr << endl;
                    break; 
                }

            }

            ADDRINT esp = PIN_GetContextReg(to, LEVEL_BASE::REG_STACK_PTR);
      
            /* The exception handling stuff will push a lot of data to the
               stack, so take account for that here. */
            PIN_SetContextReg(to, LEVEL_BASE::REG_STACK_PTR, esp-ehandler_esp_offset);
      
            PIVOT_testpivot(ps, to, *tracker);
      
            FlushBuffer(false, from, threadid, false);
            Cleanup();
            exit(1);
      
        } else {
            cerr << "Ignoring exception!" << endl;
        }
#endif
    } else if (reason == CONTEXT_CHANGE_REASON_CALLBACK) {
#if 0
        cerr << "Received windows callback" << endl;
#endif
  
    } else {
        std::cerr << "Received other exception " << reason << endl;
    }

    LLOG("done handling exception\n");
    ReleaseLock(&lock);
}

VOID FollowChild(THREADID threadid, const CONTEXT* ctxt, VOID * arg)
{
    int i;

    LLOG("follow child\n");
  
    GetLock(&lock, threadid+1);
    i = strlen(g_threadname);
    assert(i < BUFFER_SIZE);
    g_threadname[i++] = 'c';

    g_twnew = new TraceContainerWriter((g_threadname + KnobOut.Value()).c_str(), bfd_arch_i386, bfd_mach_i386_i386, default_frames_per_toc_entry, false);
  
    g_bufidx = 0;
    g_kfcount = 0;
  
    g_logcount = 0;
    g_loglimit = KnobLogLimit.Value();
  
    g_timer = clock();
    std::cerr << "Spawning child: " << PIN_GetPid() << g_threadname << std::endl;
    ReleaseLock(&lock);
  
}

bool FollowExec(CHILD_PROCESS cp, VOID *v) {
    bool follow = false;  
    int argc;
    const char * const * argv;
  
    CHILD_PROCESS_GetCommandLine(cp, &argc, &argv);
    assert (argc >= 0);
    cerr << "Exec: ";
    for (int i = 0; i < argc; i++) {
        cerr << argv[i] << " ";
    }
    cerr << endl;
  
    /* See if we should follow this */
    for (unsigned int i = 0; i < FollowProgs.NumberOfValues(); i++) {
        if (FollowProgs.Value(i) == argv[0]) {
            follow = true;
        }
    }


    if (follow)
        cerr << "Following" << endl;
    else
        cerr << "Not following" << endl;

#ifndef _WIN32
    /* If we're on Linux, this means we're about to call execv(), and
     * we're going to disappear! We had better write out our trace! */

    cerr << "Flushing buffer before execv()" << endl;
    FlushBuffer(false, NULL, PIN_ThreadId(), true);
    Cleanup();
#endif

    return follow;
}

VOID Fini(INT32 code, VOID *v)
{
    LOG("In Fini");
    Cleanup();
}

// Caller responsible for mutual exclusion
VOID Cleanup()
{
    g_twnew->finish();

    LOG("done.\n");

    clock_t endtime = clock();

    LOG("Time taken: " + decstr((UINT64) (endtime - g_timer)));

    exit(0);

}

INT32 Usage()
{
    cerr << endl << KNOB_BASE::StringKnobSummary() << endl;
    return -1;
}

void read_visited_bbs(FILE *f, set<ADDRINT> *visited_bbs){
  ADDRINT addr;
  
  while(!feof(f) && !ferror(f)){
    int r = fscanf(f, "0x%x\n", &addr);
    if(r != 1){
      break;
    }
    visited_bbs->insert(addr);
  }

  if(ferror(f)){
    assert(FALSE);
  }
}

int main(int argc, char *argv[])
{
    stringstream ss;
  
    cerr << hex;

    // A sanity check for AppendBuffer
    assert(sizeof(RPassType) == sizeof(ADDRINT));
  
    PIN_InitSymbols();

    if (PIN_Init(argc,argv))
        return Usage();

    InitLock(&lock);

    // Check if a trigger was specified.
    if (KnobTrigAddr.Value() != 0) {
        g_usetrigger = true;
        g_trig_resolved = false;

        // Set trigger countdown to initial value.
        g_trig_countdown = KnobTrigCount.Value();
      
    } else {
        g_usetrigger = false;
    }

    // Check if coverage tracking is on
    if (KnobCoverageTracking.Value())
    {
      g_logCoverage = true;
    }

    if(KnobBBFile.Value() != ""){
      g_bbfile = fopen(KnobBBFile.Value().c_str(), "wb");
      if(!g_bbfile){
        cerr << "Can't open " << KnobBBFile.Value() << " for writing." << endl;
        assert(FALSE);
      }
    }

    if(KnobVisitedBBFile.Value() != ""){
      g_visited_bb_file = fopen(KnobVisitedBBFile.Value().c_str(), "r");
      if(!g_visited_bb_file){
        cerr << "Can't open " << KnobVisitedBBFile.Value() << " for reading." 
          << endl;
        assert(FALSE);
      }
      read_visited_bbs(g_visited_bb_file, &g_visited_bbs);
      fclose(g_visited_bb_file);
    }


    // Check if taint tracking is on
    if (KnobTaintTracking.Value()) {
        tracker = new TaintTracker(values);
        for (uint32_t i = 0 ; i < TaintedFiles.NumberOfValues() ; i++) {
            if (TaintedFiles.Value(i) != "") {
                tracker->trackFile(TaintedFiles.Value(i));
            }
        }

        tracker->setTaintArgs(TaintedArgs);
        if (TaintedStdin)
            tracker->setTaintStdin();
        if (TaintedNetwork)
            tracker->setTaintNetwork();
        if (TaintedEnv.Value() != "")
            tracker->setTaintEnv(TaintedEnv.Value());
    }

    /* Get a key for thread info */
    tl_key = PIN_CreateThreadDataKey(NULL);
    assert(tl_key != -1);

    // We must activate taint tracking early if we have tainted args
    // or envs
    if ((TaintedEnv.Value() != "") || TaintedArgs.Value()) {
        g_taint_introduced = true;
    } else {
        g_taint_introduced = false;
    }

    // Determine whether logging is enabled.
    // If a trigger is specified, logging is never enabled, because
    // nothing is logged until the trigger point.  Otherwise, logging
    // is enabled if taint is introduced, or if logging before taint is enabled.
    if (g_usetrigger) {
        g_active = false;
    } else {
        if (g_taint_introduced || LogAllBeforeTaint.Value()) {
            g_active = true;
        } else {
            g_active = false;
        }
    }

    cerr << "Logging initially enabled: " << g_active << endl;

    // Make a memory snapshot?
    g_mem_snapshot = 0;
    if (SnapshotFile.Value() != "") {
      g_mem_snapshot = 1;
    }

    /** Read pivot gadgets */
    if (PivotFile.Value() != "") {
        fstream f;
        pivot_set::iterator i;

        f.open(PivotFile.Value().c_str());
        if (!f.is_open()) {
            cerr << "Could not open pivot gadget file: " << PivotFile.Value() << endl;
            exit(1);
        }

        ps = PIVOT_parseinput(f);
        cerr << "Read " << ps.size() << " pivots" << endl;

        f.close();
    }

    IMG_AddInstrumentFunction(ModLoad, 0);
    TRACE_AddInstrumentFunction(InstrTrace, 0);
    PIN_AddThreadStartFunction(ThreadStart, 0);
    PIN_AddThreadFiniFunction((THREAD_FINI_CALLBACK)ThreadEnd, 0);
   
    PIN_AddContextChangeFunction(ExceptionHandler, 0);
   
#ifndef _WIN32
    PIN_AddForkFunction(FPOINT_AFTER_IN_CHILD, FollowChild, 0);
    PIN_AddForkFunction(FPOINT_AFTER_IN_PARENT, FollowParent, 0);
#endif
    PIN_AddFollowChildProcessFunction(FollowExec, 0);
   
    PIN_AddSyscallEntryFunction(SyscallEntry, 0);
    PIN_AddSyscallExitFunction(SyscallExit, 0);
   
    PIN_AddFiniFunction(Fini, 0);

    //ss << PIN_GetPid() << "-" << KnobOut.Value();
    ss << KnobOut.Value();
   
    g_twnew = new TraceContainerWriter(ss.str().c_str(), bfd_arch_i386, bfd_mach_i386_i386, default_frames_per_toc_entry, false);

    g_bufidx = 0;
    g_kfcount = 0;
   
    g_logcount = 0;
    g_loglimit = KnobLogLimit.Value();

    g_skipTaints = SkipTaints.Value();
    g_stack_dump = StackDump.Value();

    g_timer = clock();

    g_exit_next = false;
   
    start_addr = TaintStart.Value();
    end_addr = TaintEnd.Value();

    cerr << "Code cache limit is " << CODECACHE_CacheSizeLimit() << endl;
    assert(CODECACHE_ChangeCacheLimit(CacheLimit.Value()));

    LOG("Starting program\n");
    cerr << "Starting program" << endl;

    // Start the program, never returns
    PIN_StartProgram();

    return 0;

}
