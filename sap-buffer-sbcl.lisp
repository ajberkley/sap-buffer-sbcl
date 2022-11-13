(defpackage #:sap-buffer-sbcl
  (:use #:common-lisp)
  (:import-from #:lparallel))

(in-package #:sap-buffer-sbcl)

;; Simple implementation, lock on allocate

(defvar *sap-buffer-size* (expt 2 20))

(defun new-sap-buffer (&optional (size *sap-buffer-size*))
  (values (sb-alien:make-alien (sb-alien:unsigned 8) size) size))

(defun free-sap-buffer (alien)
  (sb-alien:free-alien alien))

(defvar *results-lock* (sb-thread:make-mutex))

(defvar *results* nil)

(defun locked-foreign-buffer-flush ()
  (sb-thread:with-recursive-lock (*results-lock*)
    (push *current-buffer* *results*)
    (setf *current-buffer* (make-locked-foreign-buffer))))

(defstruct (locked-foreign-buffer (:constructor %make-locked-foreign-buffer))
  (sap nil :type (or null sb-sys:system-area-pointer))
  (offset 0 :type fixnum)
  (size 0 :type fixnum)
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (alien nil :type sb-alien:alien))

(defun make-locked-foreign-buffer (&optional (size *sap-buffer-size*))
  (let ((alien (sb-alien:make-alien (sb-alien:unsigned 8) size)))
    (%make-locked-foreign-buffer
     :sap (sb-alien:alien-sap alien)
     :offset 0
     :size size
     :alien alien)))

(defun free-results ()
  (sb-thread:with-mutex (*results-lock*)
    (map nil (lambda (locked-foreign-buffer)
               (sb-alien:free-alien (locked-foreign-buffer-alien locked-foreign-buffer)))
         *results*)
    (setf *results* nil)))

(defvar *current-buffer* (make-locked-foreign-buffer))

(defun allocate-locked (size)
    (tagbody
     try
       (let ((buf *current-buffer*))
         (sb-thread:with-mutex ((locked-foreign-buffer-lock buf))
           (if (> (- (locked-foreign-buffer-size buf)
                     (locked-foreign-buffer-offset buf))
                  size)
               (prog1
                   (locked-foreign-buffer-offset buf)
                 (incf (locked-foreign-buffer-offset buf) size))
               (progn
                 (locked-foreign-buffer-flush)
                 (go try)))))))

(defun test-locked-performance (&key (num-threads 16) (work-time 100d-9) (alloc-size 16) (num-allocs-per-thread 256) (repeat 10))
  (lparallel.kernel-util:with-temp-kernel (num-threads)
    (time
     (dotimes (r repeat)
       (lparallel:pdotimes (n num-threads)
         (declare (ignore n))
         (loop repeat num-allocs-per-thread
               do (allocate-locked alloc-size)
                  (sleep work-time))))))
  (format t "Current buffer ~A / ~A used~%"
          (locked-foreign-buffer-offset *current-buffer*)
          (locked-foreign-buffer-size *current-buffer*))
  (locked-foreign-buffer-flush)
  (format t "~A total buffers allocated~%" (length *results*))
  (free-results))

;;;;; LOCKLESS

(defvar *lockless-results* nil)
(defvar *lockless-results-lock* (sb-thread:make-mutex))

(defun free-lockless-results ()
  (sb-thread:with-mutex (*lockless-results-lock*)
    (map nil (lambda (lockless-foreign-buffer-info)
               (sb-alien:free-alien (cdr lockless-foreign-buffer-info)))
         *lockless-results*)
    (setf *lockless-results* nil)))

(defstruct (lockless-foreign-buffer (:constructor %make-lockless-foreign-buffer))
  (sap nil :type (or null sb-sys:system-area-pointer))
  (offset 0 :type (unsigned-byte 64))
  (size 0 :type (unsigned-byte 64))
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (flusher #'identity)
  (alien nil :type sb-alien:alien))

(defun make-lockless-foreign-buffer (&optional (size *sap-buffer-size*))
  (let ((alien (sb-alien:make-alien (sb-alien:unsigned 8) size)))
    (%make-lockless-foreign-buffer
     :sap (sb-alien:alien-sap alien)
     :offset 0
     :size size
     :alien alien
     :flusher 'lockless-foreign-buffer-flush)))

(defvar *current-lockless-buffer* (make-lockless-foreign-buffer))

(defun lockless-foreign-buffer-flush (buf real-size)
  ;; Flushing is a locked operation
  (sb-thread:with-recursive-lock ((lockless-foreign-buffer-lock buf))
    (push (cons real-size (lockless-foreign-buffer-alien buf)) *lockless-results*)
    (setf *current-lockless-buffer* (make-lockless-foreign-buffer))))

(defun allocate-lockless (size)
  (tagbody
   try
     (let* ((buf *current-lockless-buffer*)
            (sap (lockless-foreign-buffer-sap buf))
            (buf-size (lockless-foreign-buffer-size buf))) ;; not allowed to change
       ;; try allocate
       (let ((old-offset (sb-ext:atomic-incf (lockless-foreign-buffer-offset buf) size)))
         (when (and (not (>= old-offset buf-size)) ;; someone else hit the end before us
                    (>= (+ old-offset size) buf-size))
           (funcall (lockless-foreign-buffer-flusher buf) buf old-offset)
           (go try))
         (return-from allocate-lockless (values sap old-offset))))))

(defun test-lockless-performance (&key (num-threads 16) (work-time 100d-9) (alloc-size 16) (num-allocs-per-thread 256) (repeat 10))
  (lparallel.kernel-util:with-temp-kernel (num-threads)
    (time
     (dotimes (r repeat)
       (lparallel:pdotimes (n num-threads)
         (declare (ignore n))
         (loop repeat num-allocs-per-thread
               do (allocate-lockless alloc-size)
                  (sleep work-time))))))
  (format t "Current buffer ~A / ~A used~%"
          (lockless-foreign-buffer-offset *current-lockless-buffer*)
          (lockless-foreign-buffer-size *current-lockless-buffer*))
  (funcall (lockless-foreign-buffer-flusher *current-lockless-buffer*)
           *current-lockless-buffer*
           (lockless-foreign-buffer-offset *current-lockless-buffer*))
  (format t "~A total buffers allocated~%" (length *lockless-results*))
  (free-lockless-results))
