# sap-buffer-sbcl
Common Lisp thread safe lockless SAP buffer writing

You have a fixed size raw memory buffer, say something you are passing back and forth via FFI to some other code, which is structured as a collection of headered messages.  You want to add data to it from multiple threads without stomping all over each other and as efficiently as possible.  You can afford for some small amount of unused space which should be filled with a message type equivalent to a NOP.  When the buffer is full, you want to flush it and start a new one.

This repo provides for simple thread safe lockless bump allocation and buffer flushing.  The primary intent is to achieve high parallelization, where the simple method of locking for each allocation of space is ineffective.  Think of this also as a playground for looking at memory allocation methodologies.

There are a bunch of different approaches one can take here.  You could, for example allocate moderate sized chunks of the buffer per thread in advance and do local bump allocation (say how sbcl might manage its own memory) and fallback to a locked global allocator for each page.  This is very efficient but wastes some space.  You could return unused space when a thread ceases operating on the buffer and offer that to another thread if there is significant space left.

The first comparison is a locked buffer and a buffer with atomic allocation.  When buffers are finished both methods use a global lock.

With 1 MB buffers, and 16 byte allocation chunks, 8 threads on a slow laptop

SAP-BUFFER-SBCL> (test-lockless-performance :num-threads 8 :repeat 1000 :work-time-ns 0)
Evaluation took:
  0.275 seconds of real time
  1.849875 seconds of total run time (1.849875 user, 0.000000 system)
  672.73% CPU
  549,079,380 processor cycles
  1,281,344 bytes consed
  
Current buffer 262144 / 1048576 used
32 total buffers allocated

SAP-BUFFER-SBCL> (test-locked-performance :num-threads 8 :repeat 1000 :work-time-ns 0)

Evaluation took:
  0.539 seconds of real time
  3.787209 seconds of total run time (1.817162 user, 1.970047 system)
  702.60% CPU
  1,076,471,206 processor cycles
  1,288,944 bytes consed
  
Current buffer 262144 / 1048576 used
32 total buffers allocated

As expected, once in awhile threads get blocked on the mutex in the locking version and eat up system time.  If they just busy waited in
userspace longer they'd look more like the lockless version.
