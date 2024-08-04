;;;; frugal-uuid-v6.lisp

(in-package #:frugal-uuid)

(declaim (ftype (function (uuid) (values uuid &optional))
                make-v6-from-v1))
(defun make-v6-from-v1 (v1)
  (let ((v1-time-high-and-version (time-hi-and-version v1))
        (v1-time-mid (time-mid v1))
        (v1-time-low (time-low v1))
        (v6-time-high #x00000000)
        (v6-time-mid #x0000)
        (v6-time-low-and-version #x0000))
    ;; Rearrange the timestamp
    (setf
     ;; Put the high 32 bits of the timestamp in v6-time-high:
     (ldb (byte 12 20) v6-time-high) (ldb (byte 12 0)
                                          v1-time-high-and-version)
     (ldb (byte 16 4) v6-time-high) v1-time-mid
     (ldb (byte 4 0) v6-time-high) (ldb (byte 4 28) v1-time-low)

     ;; Put the high 16 bits of v1-time-low into v6-time-mid:
     (ldb (byte 16 0) v6-time-mid) (ldb (byte 16 12) v1-time-low)


     ;; Put the lowest bits of the timestamp in
     ;; v6-time-low-and-version and set the version to version 6
     (ldb (byte 12 0) v6-time-low-and-version) (ldb (byte 12 0) v1-time-low)
     (ldb (byte 4 12) v6-time-low-and-version) #x6)

    (make-instance 'uuid
                   :time-low v6-time-high
                   :time-mid v6-time-mid
                   :time-hi-and-version v6-time-low-and-version
                   :clock-seq-hi-and-res (clock-seq-hi-and-res v1)
                   :clock-seq-low (clock-seq-hi-and-res v1)
                   :node (node v1))))

(declaim (ftype (function (uuid) (values (unsigned-byte 32) &optional))
                v6-time-high))

(defun v6-time-high (uuid)
  (time-low uuid))

(declaim (ftype (function (uuid) (values (unsigned-byte 16) &optional))
                v6-time-mid
                v6-time-low-and-version))

(defun v6-time-mid (uuid)
  (time-mid uuid))

(defun v6-time-low-and-version (uuid)
  (time-hi-and-version uuid))

(declaim (ftype (function () (values uuid &optional)) make-v6))
(defun make-v6 ()
  "Generate uuid value (version 6).

 Implementation is based on generating a version 1 uuid value and
 reordering the timestamp. See frugal-uuid-v1.lisp for details."
  (make-v6-from-v1 (make-v1)))

(declaim (inline make-v6-integer))
(defun make-v6-integer ()
  (to-integer (make-v6)))

(declaim (inline make-v6-string))
(defun make-v6-string ()
  (to-string (make-v6)))

(declaim (inline make-v6-octets))
(defun make-v6-octets ()
  (to-octets (make-v6)))

(declaim (inline make-v6-sym))
(defun make-v6-sym ()
  (to-sym (make-v6)))
