;;;; frugal-uuid-v3.lisp

(in-package #:frugal-uuid)

(declaim (ftype (function (t) uuid) make-v3-from-integer))
(defun make-v3-from-integer (i)
  "Set the bits for version 3 and IETF variant, return uuid value."
  (setf (ldb (byte 4 76) i) #x3      ; Set version to 3
        (ldb (byte 2 62) i) #b10)    ; Set variant to IETF
  (from-integer i))

(declaim (ftype (function ((simple-array (unsigned-byte 8))) uuid)
                make-v3-from-octets))
(defun make-v3-from-octets (octets)
  "Set the bits for version 3 and IETF variant, return uuid value."
  (make-v3-from-integer (octets-to-integer octets)))

;; See non-frugal.lisp for MAKE-V3 implementation
