(defpackage #:sap-buffer-sbcl-util
  (:use #:common-lisp)
  (:import-from #:lparallel)
  (:import-from #:alexandria)
  (:export
   #:busy-wait-ns
   #:letf*
   #:ignore-symbol-package-locked-error
   #:with-collect))

(in-package :sap-buffer-sbcl-util)

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

;;; Implementation of letf and letf* attributed to CMUCL sources Circa 1994
(defmacro letf* (bindings &body body &environment env)
  "Does what one might expect, saving the old values and setting the
  generalized variables to the new values in sequence.  Unwind-protects
  and get-setf-method are used to preserve the semantics one might
  expect in analogy to let*, and the once-only evaluation of subforms."
  (if (null bindings)
      (cons 'progn body)
      (labels ((do-bindings (bindings)
                 (cond ((null bindings)
                        body)
                       (t
                        (destructuring-bind (var val) (car bindings)
                          (multiple-value-bind
                                (dummies vals newval setter getter)
                              (get-setf-expansion var env)
                            (let ((save (gensym)))
                              `((let* (,@(mapcar #'list dummies vals)
                                       (,(car newval) ,val)
                                         (,save ,getter))
                                  (unwind-protect
                                       (progn ,setter
                                              ,@(do-bindings (cdr bindings)))
                                    (setq ,(car newval) ,save)
                                    ,setter))))))))))
        (car (do-bindings bindings)))))

(defmacro ignore-symbol-package-locked-error (&body body)
  `(handler-bind ((sb-ext:symbol-package-locked-error (lambda (e)
                                                        (declare (ignore e))
                                                        (invoke-restart (find-restart :ignore-all)))))
    ,@body))

(defmacro with-collect ((target &optional (collect-verb 'collect!) (append-verb 'append!) (tail 'tail)) &body body)
  "For when loop and iterate just don't match what you want to do.

   (with-collect (result)
    (collect! (+ 1 3))
    (collect! 17)
    (append! (list 1 2 3))
    result) -> (4 17 1 2 3).

   You can also access the tail directly or append lists (faster if you have the end of the list)."
  (let ((target-real (gensym (symbol-name target))))
    `(let* ((,target-real (list nil))
            (,tail ,target-real))
       (labels ((,collect-verb (value)
                  (setf ,tail (setf (cdr ,tail) (cons value nil)))
                  (cdr ,target-real))
                (,append-verb (list &optional (last-cons (last list)))
                  (setf (cdr ,tail) list)
                  (setf ,tail last-cons)))
         (declare (ignorable #',collect-verb #',append-verb))
         (symbol-macrolet ((,target (the list (cdr ,target-real))))
           ,@body)))))
