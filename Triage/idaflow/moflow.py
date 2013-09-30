#!/usr/bin/env python

import sys, os
from datetime import datetime

#idaflow_path = os.path.dirname(__file__)
idaflow_path = 'c:/moflow/triage/idaflow'
sys.path.append(idaflow_path)
from TraceReader.tracereader import TraceReader

if __name__ == "__main__":
    print ""
    print "Moflow IDA Taint Plugin"
    print "rjohnson@sourcefire.com"
    print ""

    #trace_path = idc.AskFile(0, "*.bapflow", "Open Taint Trace")
    trace_path = "c:\\moflow\\Triage\\IDAFlow\\1f5eed2a2eb4517a0cf1099f0276060a.mov.bapflow"
    #trace_path = "c:/users/rjohnson/Downloads/out.bpt"
    start_time = datetime.now()
    reader = TraceReader(trace_path)
    stop_time = datetime.now() - start_time
    print ">>Trace file", reader.filename, "indexed", reader.frame_count, "frames in", stop_time
    print len(reader.taint_addrs), "tainted"
    print len(reader.module_frames), "modules"
    print len(reader.exception_frames), "exceptions"

    for module in reader.module_frames:
        print "Module load: %08x - %08x %s" % (module.low_address, module.high_address, module.module_name)

    for exception in reader.exception_frames:
        if exception.exception_number != 0:
            print "Exception %08x in thread %d from 0x%08x to 0x%08x" % (int(exception.exception_number), exception.thread_id, exception.from_addr, exception.to_addr)

    for tainted in reader.taint_frames:
        continue
        if tainted.address == 0x06803deb:
            print tainted;
        print "Tainted addr: %08x tid: %d" % (tainted.address, tainted.thread_id)
        for el in tainted.operand_list.elem:

            taint_str  = ''
            if el.taint_info.no_taint == False:
                taint_str = "tainted"
            op_usage = ''
            if el.operand_usage.read:
                op_usage = "read"
            elif el.operand_usage.written:
                op_usage += "write"
            elif el.operand_usage.index:
                op_usage += "index"
            elif el.operand_usage.base:
                op_usage += "base"
            if op_usage != '':
                if el.operand_info_specific.mem_operand.IsInitialized():
                    el_value = ''
                    for byte in el.value:
                        el_value += "%02x " % (ord(byte))
                    print "mem[0x%08x]:u%d = 0x%s %s %s" % (el.operand_info_specific.mem_operand.address, el.bit_length, el_value, op_usage, taint_str)
                elif el.operand_info_specific.reg_operand.IsInitialized():
                    el_value = ''
                    for byte in el.value:
                        el_value += "%02x" % (ord(byte))
                    print "%s:u%d = 0x%s %s %s" % (el.operand_info_specific.reg_operand.name, el.bit_length, el_value[::-1], op_usage, taint_str)

