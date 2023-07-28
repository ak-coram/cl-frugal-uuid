;;;; non-frugal/minara.lisp

;; MiNaRa stands for Mi(llis)-Na(nos)-Ra(ndom) encoded via version 8

(in-package #:frugal-uuid)

(defconstant +minara-max-random+ #x3FFFFFFFFFFFFF)

(declaim (ftype (function (uuid)
                          (values (unsigned-byte 48)
                                  (integer 0 999999)
                                  (unsigned-byte 54)))
                minara-components))
(defun minara-components (minara)
  "Returns the millis, nanos and random components of the uuid value."
  (let ((millis 0)
        (nanos 0)
        (random 0)
        (clock-seq-low (clock-seq-low minara)))
    (setf (ldb (byte 32 16) millis) (time-low minara)
          (ldb (byte 16 0) millis) (time-mid minara)

          (ldb (byte 12 8) nanos) (ldb (byte 12 0)
                                       (time-hi-and-version minara))
          (ldb (byte 6 2) nanos) (ldb (byte 6 0)
                                      (clock-seq-hi-and-res minara))
          (ldb (byte 2 0) nanos) (ldb (byte 2 6) clock-seq-low)

          (ldb (byte 6 48) random) (ldb (byte 6 0) clock-seq-low)
          (ldb (byte 48 0) random) (node minara))
    (values millis nanos random)))

(declaim (ftype (function ((unsigned-byte 48)
                           (integer 0 999999)
                           (unsigned-byte 54))
                          uuid)
                make-minara-from-components))
(defun make-minara-from-components (millis nanos random)
  ;; Need 20 bits to store the number of nanoseconds within a
  ;; millisecond timestamp: use the 12 bits from custom_b and 8
  ;; more high bits from custom_c.
  (let ((custom-c random))
    (setf (ldb (byte 8 54) custom-c) (ldb (byte 8 0) nanos))
    (make-v8
     ;; custom_a is used for the number of milliseconds since the
     ;; unix epoch (same as in version 7)
     millis
     (ldb (byte 12 8) nanos)
     custom-c)))

(declaim (ftype (function () uuid)
                make-minara))
(defun make-minara ()
  "Generate uuid value (custom MiNaRa version based on version 8)."
  (multiple-value-bind (seconds nanos) (funcall *unix-timestamp-function*)
    (multiple-value-bind (millis nanos) (floor nanos +nanos-per-milli+)
      (make-minara-from-components
       (+ (* seconds +millis-per-second+) millis)
       nanos
       (random-integer +minara-max-random+)))))

(defparameter *minara-min*
  (make-minara-from-components 0 0 0)
  "Smallest possible MiNaRa UUID")

(defparameter *minara-max*
  (make-minara-from-components #xFFFFFFFFFFFF
                               (1- +nanos-per-milli+)
                               +minara-max-random+)
  "Largest possible MiNaRa UUID")
