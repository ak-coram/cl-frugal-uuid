;;;; frugal-uuid-v1.lisp

(in-package #:frugal-uuid)

(defclass v1-generator ()
  ((node-id :initarg :v1-node-id
            :accessor v1-node-id
            :type (unsigned-byte 48))
   (clock-seq :initarg :v1-clock-seq
              :accessor v1-clock-seq
              :type (unsigned-byte 14))
   (timestamp-generator :initarg :v1-timestamp-generator
                        :accessor v1-timestamp-generator)))

(defvar *v1-generator* nil)

(defun make-v1-generator (&key node-id clock-seq timestamp-generator)
  (make-instance 'v1-generator
                 :v1-node-id (or node-id (random-node-id))
                 :v1-clock-seq (or clock-seq (random-clock-seq))
                 :v1-timestamp-generator (or timestamp-generator
                                             (make-timestamp-generator))))

(defun initialize-v1-generator (&optional v1-generator)
  (setf *v1-generator* (or v1-generator (make-v1-generator)))
  nil)

(defmacro with-v1-generator (v1-generator &body body)
  "Dynamically bind generator for creating version 1 uuid values."
  `(let ((*v1-generator* ,v1-generator))
     ,@body))

(declaim (ftype (function (integer) (values uuid &optional))
                make-v1-from-timestamp))
(defun make-v1-from-timestamp (timestamp)
  (unless *v1-generator* (initialize-v1-generator))
  (let ((clock-seq (v1-clock-seq *v1-generator*))
        (clock-seq-high #xFF)
        (time-high-and-version #xFFFF))
    (setf (ldb (byte 2 6) clock-seq-high) #b10 ; Set variant to IETF
          (ldb (byte 6 0) clock-seq-high) (ldb (byte 6 8) clock-seq)
          (ldb (byte 4 12) time-high-and-version) #x1 ; Set version to 1
          (ldb (byte 12 0) time-high-and-version) (ldb (byte 12 48) timestamp))
    (make-instance 'uuid
                   :time-low (ldb (byte 32 0) timestamp)
                   :time-mid (ldb (byte 16 32) timestamp)
                   :time-hi-and-version time-high-and-version
                   :clock-seq-hi-and-res clock-seq-high
                   :clock-seq-low (ldb (byte 8 0) clock-seq)
                   :node (v1-node-id *v1-generator*))))

(declaim (ftype (function () (values uuid &optional)) make-v1))
(defun make-v1 ()
  "Generate uuid value (version 1)."
  (unless *v1-generator* (initialize-v1-generator))
  (make-v1-from-timestamp (funcall (v1-timestamp-generator *v1-generator*))))

