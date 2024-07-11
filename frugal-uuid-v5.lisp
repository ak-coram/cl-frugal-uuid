;;;; frugal-uuid-v5.lisp

(in-package #:frugal-uuid)

(declaim (ftype (function (t) (values uuid &optional)) make-v5-from-integer))
(defun make-v5-from-integer (i)
  "Set the bits for version 5 and IETF variant, return uuid value."
  (setf (ldb (byte 4 76) i) #x5      ; Set version to 5
        (ldb (byte 2 62) i) #b10)    ; Set variant to IETF
  (from-integer i))

(declaim (ftype (function ((simple-array (unsigned-byte 8))) (values uuid &optional))
                make-v5-from-octets))
(defun make-v5-from-octets (octets)
  "Set the bits for version 5 and IETF variant, return uuid value."
  (make-v5-from-integer (octets-to-integer octets)))

;; See non-frugal/name-based.lisp for MAKE-V5
