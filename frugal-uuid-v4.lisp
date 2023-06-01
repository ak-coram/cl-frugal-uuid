;;;; frugal-uuid-v4.lisp

(in-package #:frugal-uuid)

(defvar *random* nil)

(defun make-v4 (&optional random-state)
  "Generate random uuid value (version 4).

Initializes own random-state when first invoked. Alternatively the
optional RANDOM-STATE parameter can used instead."
  (unless *random* (setf *random* (make-random-state t)))
  ;; Generate 128-bit random value
  (let ((base (random #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                      (or random-state *random*))))
    (setf (ldb (byte 4 76) base) #x4 ; Set version to random
          (ldb (byte 2 62) base) #b10) ; Set variant to IETF
    (from-integer base)))

