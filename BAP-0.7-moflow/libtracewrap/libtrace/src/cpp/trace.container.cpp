/**
 * $Id: trace.container.cpp 6650 2012-08-18 00:48:38Z edmcman $
 *
 * Implementation of trace container.
 */

#include "trace.container.hpp"
#include <stdio.h>
#include <iostream>
#include <string>

#define WRITE(x) { if (fwrite(&(x), sizeof(x), 1, ofs) != 1) { throw (TraceException("Unable to write to trace")); } }
#define READ(x) { if (fread(&(x), sizeof(x), 1, ifs) != 1) { throw (TraceException("Unable to read from trace")); } }
#ifdef _WIN32
typedef uint64_t traceoff_t;
#define SEEKNAME _fseeki64
#define SEEK(f,x) { if (SEEKNAME(f, x, SEEK_SET) != 0) { throw (TraceException("Unable to seek in trace to offset " + x)); } }
#define TELL(f) _ftelli64(f)
#else
typedef off_t traceoff_t;
#define SEEKNAME fseeko
#define SEEK(f,x) { if (SEEKNAME(f, x, SEEK_SET) != 0) { throw (TraceException("Unable to seek in trace to offset " + x)); } }
#define TELL(f) ftello(f)
#endif

namespace SerializedTrace {

  TraceContainerWriter::TraceContainerWriter(std::string filename,
                                             bfd_architecture arch,
                                             uint64_t machine,
                                             uint64_t frames_per_toc_entry_in,
                                             bool auto_finish_in) throw (TraceException)
    : num_frames (0),
      frames_per_toc_entry (frames_per_toc_entry_in),
      arch (arch),
      mach (machine),
      auto_finish (auto_finish_in),
      is_finished (false)
  {
    ofs = fopen(filename.c_str(), "wb");
    if (!ofs) { throw (TraceException("Unable to open trace file for writing")); }
    SEEK(ofs, first_frame_offset);
  }

  TraceContainerWriter::~TraceContainerWriter(void) throw () {

    /** Call finish if it has not been called already ANd if
        auto_finish is set. */
    if (!is_finished && auto_finish) {
      try {
        finish();
      }
      catch (std::exception const &e) {
        std::cerr << "Exception " << e.what() << " occured during TraceContainerWriter's auto-finish" << std::endl;
      }
    }
  }

  void TraceContainerWriter::add(frame &f) throw (TraceException) {
    /* Is is time for a toc entry? */
    if (num_frames > 0 && (num_frames % frames_per_toc_entry) == 0) {
      /* Yes.  Add the file offset where we will insert this frame to
         toc. */
      toc.push_back(TELL(ofs));
    }

    num_frames++;

    /* Serialize to string so we can get the length. */
    std::string s;
    if (!(f.SerializeToString(&s))) {
      throw (TraceException("Unable to serialize frame to ostream"));
    }

    /* Write the length before the frame. */
    uint64_t len = s.length();
    if (len == 0) {
      throw (TraceException("Attempt to add zero-length frame to trace"));
    }
    WRITE(len);

    /* Write the frame. */
    traceoff_t old_offset = TELL(ofs);
    if (old_offset == -1) {
      throw (TraceException("Unable to determine the current offset in the trace"));
    }

    if (fwrite(s.c_str(), 1, len, ofs) != len) {
      throw (TraceException("Unable to write frame to trace file"));
    }

    /* Double-check our size. */
    assert ((uint64_t)old_offset + len == (uint64_t)TELL(ofs));
  }

  void TraceContainerWriter::finish(void) throw (TraceException) {
    if (is_finished) {
      throw (TraceException("finish called twice"));
    }

    /* Save the offset where we will write the toc. */
    uint64_t toc_offset = TELL(ofs);
    if (toc_offset == -1) {
      throw (TraceException("Unable to determine the current offset in the trace"));
    }

    /* Make sure the toc is the right size. */
    assert ((num_frames == 0) || ((num_frames - 1) / frames_per_toc_entry) == toc.size());

    /* Write frames per toc entry. */
    WRITE(frames_per_toc_entry);

    /* Write each offset to the file. */
    for (std::vector<uint64_t>::size_type i = 0; i < toc.size(); i++) {
      WRITE(toc[i]);
    }

    /* Now we need to write the magic number, number of trace frames
       and the offset of field m at the start of the trace. */

    /* Magic number. */
    SEEK(ofs, magic_number_offset);
    WRITE(magic_number);

    /* Trace version. */
    SEEK(ofs, trace_version_offset);
    WRITE(out_trace_version);

    /* CPU architecture. */
    SEEK(ofs, bfd_arch_offset);
    uint64_t archt = (uint64_t) arch;
    WRITE(archt);

    /* Machine type. */
    SEEK(ofs, bfd_machine_offset);
    WRITE(mach);

    /* Numer of trace frames */
    SEEK(ofs, num_trace_frames_offset);
    WRITE(num_frames);

    /* Offset of toc. */
    SEEK(ofs, toc_offset_offset);
    WRITE(toc_offset);

    /* Finally, close the file and mark us as finished. */
    if (fclose(ofs)) {
      throw (TraceException("Unable to close trace file"));
    }
    is_finished = true;
  }

  bool TraceContainerWriter::has_finished(void) throw () {
    return is_finished;
  }

  TraceContainerReader::TraceContainerReader(std::string filename) throw (TraceException)
  {
    ifs = fopen(filename.c_str(), "rb");
    if (!ifs) { throw (TraceException("Unable to open trace for reading")); }

    /* Verify the magic number. */
    uint64_t magic_number_read;
    READ(magic_number_read);
    if (magic_number_read != magic_number) {
      throw (TraceException("Magic number not found in trace"));
    }

    READ(trace_version);
    if (trace_version > highest_supported_version ||
        trace_version < lowest_supported_version) {
      throw (TraceException("Unsupported trace version"));
    }

    uint64_t archt;
    READ(archt);
    arch = (bfd_architecture) archt;

    READ(mach);

    /* Read number of trace frames. */
    READ(num_frames);

    /* Find offset of toc. */
    uint64_t toc_offset;
    READ(toc_offset);

    /* Find the toc. */
    SEEK(ifs, toc_offset);

    /* Read number of frames per toc entry. */
    READ(frames_per_toc_entry);

    /* Read each toc entry. */
    for (int i = 0; i < ((num_frames - 1) / frames_per_toc_entry); i++) {
      uint64_t offset;
      READ(offset);
      toc.push_back(offset);
    }

    /* We should be at the end of the file now. */
    traceoff_t us = TELL(ifs);
    traceoff_t end = SEEKNAME(ifs, 0, SEEK_END);
    if (us != TELL(ifs) || end != 0) {
      throw(TraceException("The table of contents is malformed."));
    }

    /* Seek to the first frame. */
    seek(0);
  }

  TraceContainerReader::~TraceContainerReader(void) throw () {
    /* Nothing yet. */
  }

  uint64_t TraceContainerReader::get_num_frames(void) throw () {
    return num_frames;
  }

  uint64_t TraceContainerReader::get_frames_per_toc_entry(void) throw () {
    return frames_per_toc_entry;
  }

  bfd_architecture TraceContainerReader::get_arch(void) throw () {
    return arch;
  }

  uint64_t TraceContainerReader::get_machine(void) throw () {
    return mach;
  }

  uint64_t TraceContainerReader::get_trace_version(void) throw () {
    return trace_version;
  }

  void TraceContainerReader::seek(uint64_t frame_number) throw (TraceException) {
    /* First, make sure the frame is in range. */
    check_end_of_trace_num(frame_number, "seek() to non-existant frame");

    /* Find the closest toc entry, if any. */
    uint64_t toc_number = frame_number / frames_per_toc_entry;

    if (toc_number == 0) {
      current_frame = 0;
      SEEK(ifs, first_frame_offset);
    } else {
      current_frame = toc_number * frames_per_toc_entry;
      /* Use toc_number - 1 because there is no toc for frames [0,m). */
      SEEK(ifs, toc[toc_number - 1]);
    }

    while (current_frame != frame_number) {
      /* Read frame length and skip that far ahead. */
      uint64_t frame_len;
      READ(frame_len);
      SEEK(ifs, (uint64_t)TELL(ifs) + frame_len);
      current_frame++;
    }
  }

  std::auto_ptr<frame> TraceContainerReader::get_frame(void) throw (TraceException) {
    /* Make sure we are in bounds. */
    check_end_of_trace("get_frame() on non-existant frame");

    uint64_t frame_len;
    READ(frame_len);
    if (frame_len == 0) {
      throw (TraceException("Read zero-length frame at offset " + TELL(ifs)));
    }

    /* We really just want a variable sized array, but MS VC++ doesn't support C99 yet.
     *
     * http://stackoverflow.com/questions/5246900/enabling-vlasvariable-length-arrays-in-ms-visual-c
     */
    auto_vec<char> buf ( new char[frame_len] );

    /* Read the frame into buf. */
    if (fread(buf.get(), 1, frame_len, ifs) != frame_len) {
      throw (TraceException("Unable to read frame from trace"));
    }

    std::string sbuf(buf.get(), frame_len);

    std::auto_ptr<frame> f(new frame);
    if (!(f->ParseFromString(sbuf))) {
      throw (TraceException("Unable to parse from string"));
    }
    current_frame++;

    return f;
  }

  std::auto_ptr<std::vector<frame> > TraceContainerReader::get_frames(uint64_t requested_frames) throw (TraceException) {
    check_end_of_trace("get_frames() on non-existant frame");

    std::auto_ptr<std::vector<frame> > frames(new std::vector<frame>);
    for (uint64_t i = 0; i < requested_frames && current_frame < num_frames; i++) {
      frames->push_back(*(get_frame()));
    }

    return frames;
  }

  bool TraceContainerReader::end_of_trace(void) throw () {
    return end_of_trace_num(current_frame);
  }

  bool TraceContainerReader::end_of_trace_num(uint64_t frame_num) throw () {
    if (frame_num + 1 > num_frames) {
      return true;
    } else {
      return false;
    }
  }

  void TraceContainerReader::check_end_of_trace_num(uint64_t frame_num, std::string msg) throw (TraceException) {
    if (end_of_trace_num(frame_num)) {
      throw (TraceException(msg));
    }
  }

    void TraceContainerReader::check_end_of_trace(std::string msg) throw (TraceException) {
      return check_end_of_trace_num(current_frame, msg);
    }
};
