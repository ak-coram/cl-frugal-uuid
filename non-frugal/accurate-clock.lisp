;;;; non-frugal/accurate-clock.lisp

(in-package #:frugal-uuid)

(setf *unix-timestamp-function* #'trivial-clock:now)
