;;;; non-frugal/accurate-clock.lisp

(in-package #:frugal-uuid)

(defconstant +unix-epoch-offset-seconds+ 12219292800)

(defun get-accurate-timestamp ()
  (multiple-value-bind (seconds nanos) (trivial-clock:now)
    (+ (* (+ seconds +unix-epoch-offset-seconds+) 10000000)
       (floor nanos 100))))

(defun make-accurate-v1-generator (&key node-id clock-seq)
  (make-v1-generator :node-id node-id
                     :clock-seq clock-seq
                     :timestamp-generator #'get-accurate-timestamp))

(setf *v1-generator* nil
      *v1-generator-init-function* #'make-accurate-v1-generator)
