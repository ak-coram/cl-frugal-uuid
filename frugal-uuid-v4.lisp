;;;; frugal-uuid-v4.lisp

(in-package #:frugal-uuid)

(defvar *random* nil)

(defun make-v4 (&optional random-state)
  (unless *random* (setf *random* (make-random-state t)))
  ;; Generate 128-bit random value
  (let ((base (random #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                      (or random-state *random*))))
    (setf (ldb (byte 4 76) base) #x4 ; Set version to random
          (ldb (byte 2 62) base) 2) ; Set variant to IETF
    (from-integer base)))

