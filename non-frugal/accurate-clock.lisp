;;;; non-frugal/accurate-clock.lisp

(in-package #:frugal-uuid)

(setf *unix-timestamp-function* #'trivial-clock:now
      *v1-generator* nil
      *v7-generator* nil)
