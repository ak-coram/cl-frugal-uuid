;;;; frugal-uuid-v4.lisp

(in-package #:frugal-uuid)

(declaim (ftype (function (t) (values uuid &optional)) make-v4-from-integer))
(defun make-v4-from-integer (i)
  "Set the bits for version 4 and IETF variant, return uuid value."
  (setf (ldb (byte 4 76) i) #x4      ; Set version to random
        (ldb (byte 2 62) i) #b10)    ; Set variant to IETF
  (from-integer i))

(declaim (ftype (function () (values uuid &optional)) make-v4))
(defun make-v4 ()
  "Generate random uuid value (version 4)."
  ;; Generate 128-bit random value
  (let* ((limit #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
         (base (random-integer limit)))
    (make-v4-from-integer base)))

(declaim (inline make-v4-integer))
(defun make-v4-integer ()
  (to-integer (make-v4)))

(declaim (inline make-v4-string))
(defun make-v4-string ()
  (to-string (make-v4)))

(declaim (inline make-v4-octets))
(defun make-v4-octets ()
  (to-octets (make-v4)))

(declaim (inline make-v4-sym))
(defun make-v4-sym ()
  (to-sym (make-v4)))
