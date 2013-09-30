#ifndef TRACE_CONTAINER_HPP
#define TRACE_CONTAINER_HPP

#ifndef _WIN32
#include "config.h"
#endif
#include "arch.hpp"

/**
 * $Id: trace.container.hpp 6999 2013-01-28 19:39:01Z edmcman $
 *
 * A container for trace frames.  We do not use protobuffers because
 * protobuffers can not stream output (the whole trace would have to
 * be in memory before being written) or input (the whole trace would
 * need to be unserialized to get one frame).
 *
 * The trace format is extremely simple. All numbers are
 * little-endian.
 *
 * [<uint64_t magic number>
 *  <uint64_t trace version number>
 *  <uint64_t bfd_architecture>
 *  <uint64_t bfd_machine, 0 for unspecified>
 *  <uint64_t n = number of trace frames>
 *  <uint64_t offset of field m (below)>
 *  [ <uint64_t sizeof(trace frame 0)>
 *    <trace frame 0>
 *    ..............
 *    <uint64_t sizeof(trace frame n)>
 *    <trace frame n> ]
 *  <uint64_t m, where a table of contents entry is given
 *  for m, 2m, 3m, ..., ceil(n/m)>
 *  [ <uint64_t offset of sizeof(trace frame m)>
 *    <uint64_t offset of sizeof(trace frame 2m)>
 *    ...
 *    <uint64_t offset of sizeof(trace frame ceil(n/m))> ]]
 *
 *  One additional feature that might be nice is log_2(n) lookup
 *  time using a hierarchical toc.
 */

#include <exception>
#include <memory>
#include <stdint.h>
#include <string>
#include <vector>
#include <stdio.h>
#include "frame.piqi.pb.h"

namespace SerializedTrace {

  const uint64_t magic_number = 7456879624156307493LL;

  const uint64_t default_frames_per_toc_entry = 10000;
  const uint64_t default_auto_finish = false;
  const bfd_architecture default_arch = bfd_arch_i386;
  const uint64_t default_machine = bfd_mach_i386_i386;

  const uint64_t magic_number_offset = 0LL;
  const uint64_t trace_version_offset = 8LL;
  const uint64_t bfd_arch_offset = 16LL;
  const uint64_t bfd_machine_offset = 24LL;
  const uint64_t num_trace_frames_offset = 32LL;
  const uint64_t toc_offset_offset = 40LL;
  const uint64_t first_frame_offset = 48LL;

  const uint64_t out_trace_version = 1LL;
  const uint64_t lowest_supported_version = 1LL;
  const uint64_t highest_supported_version = out_trace_version;

    class TraceException: public std::exception
    {

    public:
      TraceException(std::string s)
        : msg (s)
        { }

      ~TraceException(void) throw () { }

      virtual const char* what() const throw()
        {
          return msg.c_str();
        }

    private:

      std::string msg;
    };

  class TraceContainerWriter {

    public:

    /** Creates a trace container writer that will output to
        [filename]. An entry will be added to the table of contents
        every [frames_per_toc_entry] entries.*/
    TraceContainerWriter(std::string filename,
                         bfd_architecture arch = default_arch,
                         uint64_t machine = default_machine,
                         uint64_t frames_per_toc_entry = default_frames_per_toc_entry,
                         bool auto_finish = default_auto_finish) throw (TraceException);

    /** Destructor that calls finish if auto_finish is true. */
    ~TraceContainerWriter(void) throw ();

    /** Add [frame] to the trace. */
    void add(frame &f) throw (TraceException);

    /** Add all frames in container [c] to the trace. */
    template <typename C>
    void add(C &c) throw (TraceException) {
      for (typename C::iterator i = c.begin(); i != c.end(); i++) {
    	add(*i);
      }
    }

    /** Finish the trace.  Builds and writes the table of contents to
     * the file. Closes the file. */
    void finish(void) throw (TraceException);

    /** Returns true iff finish() has not been called on this trace. */
    bool has_finished(void) throw ();

    protected:

    /* Output fstream for trace container file.
     *
     *  We used to use fstreams, but Windows fstreams do not allow
     *  32-bit offsets. */
    FILE *ofs;

    /** The toc entries for frames added so far. */
    std::vector<uint64_t> toc;

    /** Number of frames added to the trace. */
    uint64_t num_frames;

    /** Frames per toc entry. */
    const uint64_t frames_per_toc_entry;

    /** Architecture. */
    const bfd_architecture arch;

    /** Machine type. */
    const uint64_t mach;

    /** Call [finish()] in destructor if not already done. */
    bool auto_finish;

    /** True if [finish()] been called on this writer. */
    bool is_finished;
  };

  class TraceContainerReader {

  public:

    /** Creates a trace container reader that reads from [filename]. */
    TraceContainerReader(std::string filename) throw (TraceException);

    /** Destructor. */
    ~TraceContainerReader(void) throw ();

    /** Returns the number of frames in the trace. */
    uint64_t get_num_frames(void) throw ();

    /** Returns the number of frames per toc entry. */
    uint64_t get_frames_per_toc_entry(void) throw ();

    /** Returns the architecture of the trace. */
    bfd_architecture get_arch(void) throw ();

    /** Returns the machine type (sub-architecture) of the trace. */
    uint64_t get_machine(void) throw ();

    /** Returns trace version. */
    uint64_t get_trace_version(void) throw ();

    /** Seek to frame number [frame_number]. The frame is numbered
     * 0. */
    void seek(uint64_t frame_number) throw (TraceException);;

    /** Return the frame pointed to by the frame pointer. Advances the
        frame pointer by one after. */
    std::auto_ptr<frame> get_frame(void) throw (TraceException);

    /** Return [num_frames] starting at the frame pointed to by the
        frame pointer. If there are not that many frames until the end
        of the trace, returns all until the end of the trace.  The
        frame pointer is set one frame after the last frame returned.
        If the last frame returned is the last frame in the trace, the
        frame pointer will point to an invalid frame. */
    std::auto_ptr<std::vector<frame> > get_frames(uint64_t num_frames) throw (TraceException);

    /** Return true if frame pointer is at the end of the trace. */
    bool end_of_trace(void) throw ();

  protected:
    /** File to read trace from. */
    FILE *ifs;

    /** The toc entries from the trace. */
    std::vector<uint64_t> toc;

    /** Trace version. */
    uint64_t trace_version;

    /** Number of frames in the trace. */
    uint64_t num_frames;

    /** Number of frames per toc entry. */
    uint64_t frames_per_toc_entry;

    /** CPU architecture. */
    bfd_architecture arch;

    /** Machine type. */
    uint64_t mach;

    /** Current frame number. */
    uint64_t current_frame;

    /** Return true if [frame_num] is at the end of the trace. */
    bool end_of_trace_num(uint64_t frame_num) throw ();

    /** Raise exception if [frame_num] is at the end of the trace. */
    void check_end_of_trace_num(uint64_t frame_num, std::string msg) throw (TraceException);

    /** Raise exception if frame pointer is at the end of the trace. */
    void check_end_of_trace(std::string msg) throw (TraceException);

  };

  /* A minimal smart pointer class for arrays. */
  template< typename T_ >
  struct auto_vec{
    T_* t_;
    auto_vec( T_* t ): t_( t ) {}
    ~auto_vec() { delete[] t_; }
    T_* get() const { return t_; }
    T_* operator->() const { return get(); }
    T_& operator*() const { return *get(); }
  };

};

#endif
