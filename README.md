# sap-buffer-sbcl
Common Lisp thread safe lockless allocations from fixed buffers (specialized for SBCL alien and saps)

You have a fixed size raw memory buffer, say something you are passing back and forth via FFI to some other code.  You want to add data to it from multiple threads without stomping all over each other and as efficiently as possible.  When the buffer is full, you want to flush it somewhere and start a new one.

This repo provides for simple thread safe lockless bump allocation and buffer flushing.  The primary intent is to achieve high parallelization with many writers to the buffer, where the simple method of locking for each allocation of space is ineffective.  Think of this also as a playground for looking at memory allocation methodologies.

There are a bunch of different approaches one can take here.  You could, for example allocate moderate sized pages of the buffer per thread in advance and do thread local pointer bump allocation (say how sbcl works) and fallback to a locked global allocator for each page.  This is very fast but wastes some space.

Let's start simple though and compare a version with a global lock per allocation versus an atomic increment of a buffer pointer.  When buffers are full, both methods use a global lock to get a new buffer.

With 1 MB buffers, and 16 byte allocation chunks and 256000 allocations per thread, 8 threads on a slow laptop

    SAP-BUFFER-SBCL> (test-lockless-performance :num-threads 8 :alloc-size 16 :num-allocs-per-thread (* 1000 256) :work-time-ns 0)
     Evaluation took:
      0.075 seconds of real time
      0.583874 seconds of total run time (0.583874 user, 0.000000 system)
      778.67% CPU
      155,124,370 processor cycles
      0 bytes consed
  
     Current buffer 262144 / 1048576 used
     32 total buffers allocated for 33554432 bytes total used

    SAP-BUFFER-SBCL> (test-locked-performance :num-threads 8 :alloc-size 16 :num-allocs-per-thread (* 1000 256) :work-time-ns 0)
     Evaluation took:
      0.475 seconds of real time
      3.348583 seconds of total run time (1.507508 user, 1.841075 system)
      705.05% CPU
      949,669,435 processor cycles
      784 bytes consed
  
    Current buffer 262144 / 1048576 used
    32 total buffers allocated for 33554432 bytes total used

As expected, once in awhile threads get blocked on the mutex in the locking version and eat up system time.

Until the work-time spent outside the lock approaches the time taken in the system call the lockless version wins handily.

    SAP-BUFFER-SBCL> (test-lockless-performance :num-threads 8 :alloc-size 16 :num-allocs-per-thread (* 1000 256) :work-time-ns 2000)
     Evaluation took:
      0.559 seconds of real time
      4.363609 seconds of total run time (4.363609 user, 0.000000 system)
      780.68% CPU
      1,116,529,606 processor cycles
      0 bytes consed
  
    Current buffer 262144 / 1048576 used
    32 total buffers allocated for 33554432 bytes total used

    SAP-BUFFER-SBCL> (test-locked-performance :num-threads 8 :alloc-size 16 :num-allocs-per-thread (* 1000 256) :work-time-ns 2000)
     Evaluation took:
      0.607 seconds of real time
      4.690721 seconds of total run time (4.009296 user, 0.681425 system)
      772.82% CPU
      1,213,595,883 processor cycles
      0 bytes consed
  
    Current buffer 262144 / 1048576 used
    32 total buffers allocated for 33554432 bytes total used
