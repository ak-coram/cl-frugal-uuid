;;;; frugal-uuid-v8.lisp

(in-package #:frugal-uuid)

(declaim (ftype (function ((unsigned-byte 48)
                           (unsigned-byte 12)
                           (unsigned-byte 62))
                          (values uuid &optional))
                make-v8))

(defun make-v8 (a b c)
  (let ((time-high #xFFFF)
        (clock-seq-high #xFF))
    (setf (ldb (byte 4 12) time-high) #x8 ; Set version to 8
          (ldb (byte 12 0) time-high) b
          (ldb (byte 2 6) clock-seq-high) #b10
          (ldb (byte 6 0) clock-seq-high) (ldb (byte 6 56) c))
    (make-instance 'uuid
                   :time-low (ldb (byte 32 16) a)
                   :time-mid (ldb (byte 16 0) a)
                   :time-hi-and-version time-high
                   :clock-seq-hi-and-res clock-seq-high
                   :clock-seq-low (ldb (byte 8 48) c)
                   :node (ldb (byte 48 0) c))))
