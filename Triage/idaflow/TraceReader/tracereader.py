import struct
import bap_protobuf

# arch defines
bfd_arch_i386 = 9
bfd_mach_i386_i386 = 1

# header values
magic_number = 7456879624156307493L
lowest_supported_version = 1
highest_supported_version = 1

magic_number_offset = 0
trace_version_offset = 8
bfd_arch_offset = 16
bfd_machine_offset = 24
num_trace_frames_offset = 32
toc_offset_offset = 40
first_frame_offset = 48

class TraceReader(object):
    filename = ""
    trace = None
    magic = None
    version = None
    arch = None
    mach = None
    frame_count = None
    toc_offset = None
    toc_entries = list()

    frames = list()






    module_frames = list()
    exception_frames = list()
    taint_intro_frames = list()
    taint_frame_offset_by_addr = dict()
    taint_addrs = list()
    taint_frames = list()
    taint_frame_by_addr = dict()

    def print_trace(self):
        for offset in self.toc_entries:
            for idx in range(0, 1):
                next

    def get_frame_by_offset(self, offset, len):
        self.trace.seek(offset)
        data = self.trace.read(len)
        bap_frame = bap_protobuf.frame()
        bap_frame.ParseFromString(data)
        return bap_frame


    def get_frame_by_index(self, index):
        offset, len = self.frames[index]
        get_frame_by_offset(offset, len)


    def parse_trace(self):
        module_idx = list()
        bap_frame = bap_protobuf.frame()

        debug_count = 0
        for offset, frame_len in self.frames:
            debug_count += 1
            #if debug_count > 2000:
            #    break
            self.trace.seek(offset)
            #data = self.trace.read(frame_len)
            #bap_frame.ParseFromString(data)
            bap_frame.ParseFromString(self.trace.read(frame_len))
            #print bap_frame

            if bap_frame.modload_frame.IsInitialized():
                #modload_frame = bap_frame.modload_frame
                #print "module", modload_frame.module_name
                #self.module_frames.append([offset, frame_len])
                #if module_idx.count(bap_frame.modload_frame.low_address) == 0:
                if bap_frame.modload_frame.low_address not in module_idx:
                    self.module_frames.append(bap_frame.modload_frame)
                    module_idx.append(bap_frame.modload_frame.low_address)
            elif bap_frame.exception_frame.IsInitialized():
                #self.exception_frames.append([offset, frame_len])
                if bap_frame.exception_frame.exception_number != 0:
                    self.exception_frames.append(bap_frame.exception_frame)
            elif bap_frame.taint_intro_frame.IsInitialized():
                #self.taint_intro_frames.append([offset, frame_len])
                self.taint_intro_frames.append(bap_frame.taint_intro_frame)
            elif bap_frame.std_frame.IsInitialized():
                self.taint_frame_offset_by_addr[bap_frame.std_frame.address] = [offset, frame_len]
                self.taint_addrs.append(bap_frame.std_frame.address)
                self.taint_frames.append(bap_frame.std_frame)
                self.taint_frame_by_addr[bap_frame.std_frame.address] = bap_frame.std_frame
            else:
                print "unhandled frame:", bap_frame


    def index_trace(self):
        # build TOC;
        self.trace.seek(self.toc_offset)
        frames_per_toc, = struct.unpack("<Q", self.trace.read(8))
        #print "TOC offset:", self.toc_offset
        #print "Frames per TOC:", frames_per_toc

        if self.frame_count < frames_per_toc:
            self.toc_entries.append([first_frame_offset, self.frame_count])
        else:
            self.toc_entries.append([first_frame_offset, frames_per_toc])
        for i in range(0, ((self.frame_count - 1) / frames_per_toc)):
            toc_offset, = struct.unpack("<Q", self.trace.read(8))
            if ((i + 2) * frames_per_toc) > self.frame_count:
                frame_count = self.frame_count - ((i + 1) * frames_per_toc)
            else:
                frame_count = frames_per_toc
            self.toc_entries.append([toc_offset, frame_count])


        #DEBUG
        #if self.frame_count <
        # index frames in each TOC
        for offset, count in self.toc_entries:
            self.trace.seek(offset)
            for i in range(0, count):
                frame_len, = struct.unpack("<Q", self.trace.read(8))
                offset += 8
                #print "offset:", offset, "frame:", i, "len:", frame_len
                self.frames.append([offset, frame_len])
                offset += frame_len
                self.trace.seek(offset)

        if offset != self.toc_offset:
            raise UserWarning, "indexing did not reach end of file"

        self.trace.seek(0)


    def verify_trace(self):
        #verify header
        magic, version, arch, mach, frame_count, toc_offset = struct.unpack("<QQQQQQ", self.trace.read(48))

        if magic != magic_number:
            raise UserWarning, "Invalid Trace Format"

        if version > highest_supported_version | version < lowest_supported_version:
            raise UserWarning, "Invalid Trace Version"

        if arch != bfd_arch_i386 | mach != bfd_mach_i386_i386:
            raise UserWarning, "Unsupported Architecture"

        self.trace.seek(0)

        self.magic = magic
        self.version = version
        self.arch = arch
        self.mach = mach
        self.frame_count = frame_count
        self.toc_offset = toc_offset

    def open_trace(self, filename):
        self.filename = filename
        self.trace = open(self.filename, "rb")

    def __init__(self, filename):
        self.open_trace(filename)
        #print "verifying trace"
        self.verify_trace()
        #print "indexing trace"
        self.index_trace()
        #print "parsing trace"
        self.parse_trace()

