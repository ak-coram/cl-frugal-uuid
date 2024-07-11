;;;; frugal-uuid-v7.lisp

(in-package #:frugal-uuid)

;; Based on IETF draft:
;; https://www.ietf.org/archive/id/draft-ietf-uuidrev-rfc4122bis-08.txt

(defconstant +v7-random-counter-max+ #x3FFFFFFFFFFFFFFFFFF)

(defclass v7-generator ()
  ((clock-seq :initarg :v7-random-counter
              :accessor v7-random-counter
              :type (unsigned-byte 74))
   (timestamp-generator :initarg :v7-timestamp-generator
                        :accessor v7-timestamp-generator)))

(defun v7-random ()
  ;; Only use 72 bits of the 74 available to minimize rollover:
  (random-integer #xFFFFFFFFFFFFFFFFFF))

(defun v7-random-small ()
  ;; Only use 60 bits of the 62 available to minimize rollover:
  (random-integer #xFFFFFFFFFFFFFFF))

(defun truncate-sub-ms (nanos)
  (multiple-value-bind (millis nanos) (floor nanos +nanos-per-milli+)
    (+ (* millis +nanos-per-milli+)
       ;; Only 12 bits available for sub-millisecond precision:
       (floor (* #xFFF nanos) +nanos-per-milli+))))

(defun make-v7-generator (&key timestamp-generator)
  (make-instance 'v7-generator
                 :v7-random-counter 0
                 :v7-timestamp-generator (or timestamp-generator
                                             (make-timestamp-generator
                                              :uuid-epoch nil
                                              :make-fraction-function
                                              #'truncate-sub-ms))))

(defvar *v7-generator* nil)
(defvar *v7-generator-init-function* #'make-v7-generator)

(defun initialize-v7-generator (&optional v7-generator)
  (setf *v7-generator* (or v7-generator
                           (funcall *v7-generator-init-function*)))
  nil)

(defmacro with-v7-generator (v7-generator &body body)
  "Dynamically bind generator for creating version 7 uuid values."
  `(let ((*v7-generator* ,v7-generator))
     ,@body))

(declaim (ftype (function (integer
                           (unsigned-byte 74))
                          (values uuid &optional))
                make-v7-from-timestamp))
(defun make-v7-from-timestamp (timestamp data)
  (let ((clock-seq-high #xFF)
        (time-high-and-version #xFFFF))
    (setf (ldb (byte 4 12) time-high-and-version) #x7 ; Set version to 7
          (ldb (byte 12 0) time-high-and-version) (ldb (byte 12 62) data)
          (ldb (byte 2 6) clock-seq-high) #b10 ; Set variant to IETF
          (ldb (byte 3 3) clock-seq-high) (ldb (byte 3 59) data)
          (ldb (byte 3 0) clock-seq-high) (ldb (byte 3 56) data))
    (make-instance 'uuid
                   ;; Set the timestamp
                   :time-low (ldb (byte 32 16) timestamp)
                   :time-mid (ldb (byte 16 0) timestamp)
                   ;; Contains version and high 12 bits from clock-seq
                   :time-hi-and-version time-high-and-version
                   ;; Contains variant, low 6 bits of clock-seq
                   :clock-seq-hi-and-res clock-seq-high
                   :clock-seq-low (ldb (byte 8 48) data)
                   :node (ldb (byte 48 0) data))))

(declaim (ftype (function () (values uuid &optional)) make-v7))
(defun make-v7 ()
  "Generate uuid value (version 7)."
  (unless *v7-generator* (initialize-v7-generator))
  (multiple-value-bind (base fraction repetitions)
      (funcall (v7-timestamp-generator *v7-generator*))
    ;; Reinitialize random counter when necessary
    (if (or (null repetitions)       ; Time went backwards
            (zerop repetitions))     ; New tick
        ;; Reinitialize with random value
        (setf (v7-random-counter *v7-generator*) 
              (if fraction
                  (v7-random-small)
                  (v7-random)))
        ;; Increment counter
        (setf (v7-random-counter *v7-generator*)
              (mod (+ (v7-random-counter *v7-generator*)
                      (random-integer #xFFFF))
                   +v7-random-counter-max+)))
    (let ((data (v7-random-counter *v7-generator*)))
      (if fraction
          (multiple-value-bind (millis rest)
              (floor fraction +nanos-per-milli+)
            ;; Use the max allowed 12 bits of subsecond precision
            (setf (ldb (byte 12 62) data) rest)
            (make-v7-from-timestamp (+ (* base +millis-per-second+)
                                       millis)
                                    data))
          (make-v7-from-timestamp (* base +millis-per-second+)
                                  data)))))
