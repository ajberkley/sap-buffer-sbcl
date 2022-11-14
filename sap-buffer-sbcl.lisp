(defpackage #:sap-buffer-sbcl
  (:use #:common-lisp)
  (:import-from #:lparallel)
  (:import-from #:sap-buffer-sbcl-util #:busy-wait-ns #:ignore-symbol-package-locked-error #:letf* #:with-collect))

(in-package #:sap-buffer-sbcl)

(defun new-alien (bytes)
  (sb-alien:make-alien (sb-alien:unsigned 8) bytes))

(defun free-alien (alien)
  (sb-alien:free-alien alien))

(defun alien-sap (alien)
  (sb-alien:alien-sap alien))

(defstruct (locked-foreign-buffer (:constructor %make-locked-foreign-buffer))
  (sap nil :type (or null sb-sys:system-area-pointer))
  (offset 0 :type fixnum) ;; next free byte
  (size-bytes 0 :type fixnum)
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (alien nil :type sb-alien:alien))

(defun make-locked-foreign-buffer (bytes)
  (let ((alien (new-alien bytes)))
    (%make-locked-foreign-buffer
     :sap (alien-sap alien)
     :offset 0
     :size-bytes bytes
     :alien alien)))

(defvar *sap-buffer-bytes* (expt 2 20))

(defun new-buffer (locked-foreign-buffer &optional (bytes *sap-buffer-bytes*))
  (let ((new-alien (new-alien bytes)))
    (setf (locked-foreign-buffer-alien locked-foreign-buffer) new-alien)
    (setf (locked-foreign-buffer-offset locked-foreign-buffer) 0)
    (setf (locked-foreign-buffer-size-bytes locked-foreign-buffer) bytes)
    (setf (locked-foreign-buffer-sap locked-foreign-buffer) (alien-sap new-alien)))
  locked-foreign-buffer)

(declaim (inline allocate-locked))
(defun allocate-locked (size get-current-buffer flush-current-buffer)
  (tagbody
   try
     (let ((buf (funcall get-current-buffer)))
       (sb-thread:with-mutex ((locked-foreign-buffer-lock buf))
         (if (<= (the fixnum (+ (locked-foreign-buffer-offset buf) size))
                 (locked-foreign-buffer-size-bytes buf))
             (prog1
                 (locked-foreign-buffer-offset buf)
               (incf (locked-foreign-buffer-offset buf) size))
             (progn
               (funcall flush-current-buffer (cons (locked-foreign-buffer-offset buf) (locked-foreign-buffer-alien buf)))
               (new-buffer buf)
               (go try)))))))

(defun test-locked-performance (&key (num-threads 16) (work-time-ns 10) (alloc-size 16) (num-allocs-per-thread 256) (repeat 10))
  (declare (optimize speed safety) (type fixnum num-threads num-allocs-per-thread repeat alloc-size))
  (ignore-symbol-package-locked-error
    (letf* (((symbol-function 'sb-thread::check-deadlock) (constantly t))) ;; cons'es a lot
      (let* ((current-buffer (make-locked-foreign-buffer *sap-buffer-bytes*))
             (get-current-buffer (lambda () current-buffer)))
        (with-collect (finished-buffers collect!)
          (lparallel.kernel-util:with-temp-kernel (num-threads)
            (time
             (dotimes (r repeat)
               (lparallel:pdotimes (n num-threads)
                 (declare (ignore n))
                 (loop repeat num-allocs-per-thread
                       do (allocate-locked alloc-size get-current-buffer #'collect!)
                          (busy-wait-ns work-time-ns))))))
          (format t "Current buffer ~A / ~A used~%"
                  (locked-foreign-buffer-offset current-buffer)
                  (locked-foreign-buffer-size-bytes current-buffer))
          (collect! (cons (locked-foreign-buffer-offset current-buffer) (locked-foreign-buffer-alien current-buffer)))
          (format t "~A total buffers allocated~%" (length finished-buffers))
          (map nil (lambda (lfb) (free-alien (cdr lfb)))
               finished-buffers))))))

;;;;; LOCKLESS

(defstruct (lockless-foreign-buffer (:constructor %make-lockless-foreign-buffer))
  (sap nil :type (or null sb-sys:system-area-pointer))
  (offset 0 :type (unsigned-byte 64)) ;; next free byte
  (size 0 :type (unsigned-byte 64))
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (alien nil :type sb-alien:alien))

(defun make-lockless-foreign-buffer (&optional (size *sap-buffer-bytes*))
  (let ((alien (new-alien size)))
    (%make-lockless-foreign-buffer
     :sap (alien-sap alien)
     :offset 0
     :size size
     :alien alien)))

(declaim (inline allocate-lockless))
(defun allocate-lockless (size get-current-buffer flush-buffer)
  (declare (optimize speed safety))
  (tagbody
   try
     (let* ((buf (funcall get-current-buffer))
            (sap (lockless-foreign-buffer-sap buf))
            (buf-size (lockless-foreign-buffer-size buf))) ;; not allowed to change
       ;; try allocate
       (let ((old-offset (sb-ext:atomic-incf (lockless-foreign-buffer-offset buf) size)))
         (declare (type fixnum old-offset))
         (when (> old-offset buf-size) ;; someone else hit the end before us and is flushing the buffer
           (go try))
         (when (> (the fixnum (+ old-offset size)) buf-size) ;; we are out of space
           (funcall flush-buffer (cons old-offset buf))
           (go try))
         (return-from allocate-lockless (values sap (the fixnum (+ size old-offset))))))))

(defun test-lockless-performance (&key (num-threads 16) (work-time-ns 10) (alloc-size 16) (num-allocs-per-thread 256) (repeat 10))
  (declare (optimize speed safety) (type fixnum repeat num-allocs-per-thread alloc-size))
  (let* ((current-buffer (make-lockless-foreign-buffer *sap-buffer-bytes*))
         (lock (sb-thread:make-mutex))) ;; for flushing buffers
    (labels ((get-current-buffer () current-buffer))
      (declare (inline get-current-buffer))
      (with-collect (finished-buffers collect!)
        (labels ((locked-flush (buffer)
                   (sb-thread:with-mutex (lock)
                     (collect! buffer)
                     (setf current-buffer (make-lockless-foreign-buffer *sap-buffer-bytes*)))))
          (lparallel.kernel-util:with-temp-kernel (num-threads)
            (time
             (dotimes (r repeat)
               (lparallel:pdotimes (n num-threads)
                 (declare (ignore n))
                 (loop repeat num-allocs-per-thread
                       do (allocate-lockless alloc-size #'get-current-buffer #'locked-flush)
                          (busy-wait-ns work-time-ns))))))
          (format t "Current buffer ~A / ~A used~%"
                  (lockless-foreign-buffer-offset current-buffer)
                  (lockless-foreign-buffer-size current-buffer))
          (collect! (cons (lockless-foreign-buffer-offset current-buffer) current-buffer))
          (format t "~A total buffers allocated~%" (length finished-buffers))
          (map nil (lambda (lockless-foreign-buffer-info)
                     (sb-alien:free-alien (lockless-foreign-buffer-alien (cdr lockless-foreign-buffer-info))))
               finished-buffers))))))
