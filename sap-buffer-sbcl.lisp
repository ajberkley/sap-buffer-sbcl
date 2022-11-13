(defpackage #:sap-buffer-sbcl
  (:use #:common-lisp)
  (:import-from #:lparallel))

(in-package #:sap-buffer-sbcl)

(sb-alien:define-alien-type clockid-t (sb-alien:signed 32))
(defconstant +clock-monotonic+ 1)

(declaim (inline clock-gettime))
(sb-alien:define-alien-routine clock-gettime sb-alien:int
  (clockid clockid-t :in)
  (sp (* (sb-alien:struct sb-unix::timespec)) :in))

(declaim (inline get-internal-real-time-ns))
(defun get-internal-real-time-ns ()
  (declare (optimize speed))
  (declare (inline clock-gettime))
  (sb-alien:with-alien ((sp (sb-alien:struct sb-unix::timespec)))
    (clock-gettime +clock-monotonic+ (sb-alien:addr sp))
    (let ((sec (sb-alien:slot sp 'sb-unix::tv-sec))
          (nsec (sb-alien:slot sp 'sb-unix::tv-nsec)))
      (declare (type (integer 0 999999999) nsec))
      (declare (type (unsigned-byte 32) sec))
      (+ (* sec 1000000000) nsec))))

(defun busy-wait-ns (ns)
  ;; not good for less than 100 ns or so
  (let ((end-time (+ ns (get-internal-real-time-ns))))
    (loop while (< (get-internal-real-time-ns) end-time))))

;; Simple implementation, lock on allocate

(defvar *sap-buffer-size* (expt 2 20))

(defun new-sap-buffer (&optional (size *sap-buffer-size*))
  (values (sb-alien:make-alien (sb-alien:unsigned 8) size) size))

(defun free-sap-buffer (alien)
  (sb-alien:free-alien alien))

(defvar *results-lock* (sb-thread:make-mutex))

(defvar *results* nil
  "A list of (cons used-size alien)")

(defun locked-foreign-buffer-flush (buf)
  (sb-thread:with-recursive-lock (*results-lock*)
    (push (cons (locked-foreign-buffer-offset buf) (locked-foreign-buffer-alien buf)) *results*)
    (multiple-value-bind (alien size)
        (new-sap-buffer)
    (setf (locked-foreign-buffer-alien buf) alien)
    (setf (locked-foreign-buffer-offset buf) 0)
    (setf (locked-foreign-buffer-size buf) size))))

(defstruct (locked-foreign-buffer (:constructor %make-locked-foreign-buffer))
  (sap nil :type (or null sb-sys:system-area-pointer))
  (offset 0 :type fixnum) ;; next free byte
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
    (map nil (lambda (locked-foreign-buffer-info)
               (sb-alien:free-alien (cdr locked-foreign-buffer-info)))
         *results*)
    (setf *results* nil)))

(defvar *current-buffer* (make-locked-foreign-buffer))

(defun allocate-locked (size)
    (tagbody
     try
       ;; This could be a buffer which is already flushed!
       (let ((buf *current-buffer*))
         (sb-thread:with-mutex ((locked-foreign-buffer-lock buf))
           (if (<= (+ (locked-foreign-buffer-offset buf) size)
                   (locked-foreign-buffer-size buf))
               (prog1
                   (locked-foreign-buffer-offset buf)
                 (incf (locked-foreign-buffer-offset buf) size))
               (progn
                 (locked-foreign-buffer-flush buf)
                 (go try)))))))

(defun test-locked-performance (&key (num-threads 16) (work-time-ns 10) (alloc-size 16) (num-allocs-per-thread 256) (repeat 10))
  (let ((old-func (symbol-function 'sb-thread::check-deadlock)))
    (unwind-protect
         (progn
           (handler-bind ((sb-ext:symbol-package-locked-error (lambda (e)
                                                                (declare (ignore e))
                                                                (invoke-restart (find-restart :ignore-all)))))
             (setf (symbol-function 'sb-thread::check-deadlock) (constantly t)))
           (lparallel.kernel-util:with-temp-kernel (num-threads)
             (time
              (dotimes (r repeat)
                (lparallel:pdotimes (n num-threads)
                  (declare (ignore n))
                  (loop repeat num-allocs-per-thread
                        do (allocate-locked alloc-size)
                           (busy-wait-ns work-time-ns))))))
           (format t "Current buffer ~A / ~A used~%"
                   (locked-foreign-buffer-offset *current-buffer*)
                   (locked-foreign-buffer-size *current-buffer*))
           (locked-foreign-buffer-flush *current-buffer*)
           (format t "~A total buffers allocated~%" (length *results*))
           (free-results))
      (handler-bind ((sb-ext:symbol-package-locked-error (lambda (e)
                                                           (declare (ignore e))
                                                           (invoke-restart (find-restart :ignore-all)))))
        (setf (symbol-function 'sb-thread::check-deadlock) old-func)))))

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
  (offset 0 :type (unsigned-byte 64)) ;; next free byte
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
         (when (> old-offset buf-size) ;; someone else hit the end before us and is flushing the buffer
           (go try))
         (when (> (+ old-offset size) buf-size) ;; we are out of space
           (funcall (lockless-foreign-buffer-flusher buf) buf old-offset)
           (go try))
         (return-from allocate-lockless (values sap (+ size old-offset)))))))

(defun test-lockless-performance (&key (num-threads 16) (work-time-ns 10) (alloc-size 16) (num-allocs-per-thread 256) (repeat 10))
  (lparallel.kernel-util:with-temp-kernel (num-threads)
    (time
     (dotimes (r repeat)
       (lparallel:pdotimes (n num-threads)
         (declare (ignore n))
         (loop repeat num-allocs-per-thread
               do (allocate-lockless alloc-size)
                  (busy-wait-ns work-time-ns))))))
  (format t "Current buffer ~A / ~A used~%"
          (lockless-foreign-buffer-offset *current-lockless-buffer*)
          (lockless-foreign-buffer-size *current-lockless-buffer*))
  (funcall (lockless-foreign-buffer-flusher *current-lockless-buffer*)
           *current-lockless-buffer*
           (lockless-foreign-buffer-offset *current-lockless-buffer*))
  (format t "~A total buffers allocated~%" (length *lockless-results*))
  (free-lockless-results))
