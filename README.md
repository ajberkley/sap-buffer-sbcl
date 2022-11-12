# sap-buffer-sbcl
Common lisp thread safe lockless SAP buffer writing

You have a fixed size raw memory buffer, say something you are passing back and forth via FFI to some other code, which is structured as a collection of headered messages.  You want to add data to it from multiple threads without stomping all over each other and as efficiently as possible.  You can afford for some small amount of unused space which should be filled with a message type equivalent to a NOP.  When the buffer is full, you want to flush it and start a new one.

This repo provides for simple thread safe lockless bump allocation and buffer flushing.  The primary intent is to achieve high parallelization, where the simple method of locking for each allocation of space is ineffective.  Think of this also as a playground for looking at memory allocation methodologies.


There are a bunch of different approaches one can take here.  You could, for example allocate moderate sized chunks of the buffer per thread in advance and do local bump allocation (say how sbcl might manage its own memory) and fallback to a locked global allocator for each page.  This is very efficient but wastes some space.  You could return unused space when a thread ceases operating on the buffer and offer that to another thread if there is significant space left.
