;;;; frugal-uuid-v4.lisp

(in-package #:frugal-uuid)

(declaim (ftype (function (t) uuid) make-v4-from-integer))
(defun make-v4-from-integer (i)
  "Set the bits for version 4 and IETF variant, return uuid value."
  (setf (ldb (byte 4 76) i) #x4      ; Set version to random
        (ldb (byte 2 62) i) #b10)    ; Set variant to IETF
  (from-integer i))

(declaim (ftype (function () uuid) make-v4))
(defun make-v4 ()
  "Generate random uuid value (version 4)."
  ;; Generate 128-bit random value
  (let* ((limit #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
         (base (random-integer limit)))
    (make-v4-from-integer base)))
