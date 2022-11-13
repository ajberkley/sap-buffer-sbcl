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

Until the work-time spent outside the lock approaches the time taken in the system call (about 1 us) the lockless version wins.  After that it doesn't matter anymore.  I think the threads end up synchronized nicely at that point.  6/8 threads are working while two are fighting for a lock?  I don't have this worked out in my head yet.

    SAP-BUFFER-SBCL> (test-lockless-performance :num-threads 8 :repeat 1000 :work-time-ns 1000)
     Evaluation took:
     0.552 seconds of real time
     3.960369 seconds of total run time (3.960369 user, 0.000000 system)
     717.39% CPU
     1,105,923,517 processor cycles
     1,263,616 bytes consed
  
    Current buffer 262144 / 1048576 used
    32 total buffers allocated

    SAP-BUFFER-SBCL> (test-locked-performance :num-threads 8 :repeat 1000 :work-time-ns 1000)
     Evaluation took:
     0.684 seconds of real time
     4.604495 seconds of total run time (4.191363 user, 0.413132 system)
     673.10% CPU
     1,365,104,345 processor cycles
     1,240,560 bytes consed
  
    Current buffer 262144 / 1048576 used
    32 total buffers allocated
