;;;; frugal-uuid-v7.lisp

(in-package #:frugal-uuid)

;; Based on IETF draft:
;; https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html

;; Work with a 18 bit counter, which should allow for 262,143,000
;; unique values per second without considering the random bits:
(defconstant +v7-clock-seq-max+ #b111111111111111111)

(defclass v7-generator ()
  ((clock-seq :initarg :v7-clock-seq
              :accessor v7-clock-seq
              :type (unsigned-byte 18))
   (timestamp-generator :initarg :v7-timestamp-generator
                        :accessor v7-timestamp-generator)))

(defun make-v7-generator (&key clock-seq timestamp-generator)
  (make-instance 'v7-generator
                 :v7-clock-seq (or clock-seq
                                   (random-integer +v7-clock-seq-max+))
                 :v7-timestamp-generator (or timestamp-generator
                                             (make-timestamp-generator
                                              :uuid-epoch nil
                                              :low-resolution nil))))

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

(declaim (ftype (function (integer integer) (values uuid &optional))
                make-v7-from-timestamp))
(defun make-v7-from-timestamp (timestamp random)
  (let ((clock-seq (v7-clock-seq *v7-generator*))
        (clock-seq-high #xFF)
        (time-high-and-version #xFFFF))
    (setf (ldb (byte 4 12) time-high-and-version) #x7 ; Set version to 7
          (ldb (byte 12 0) time-high-and-version) (ldb (byte 12 6) clock-seq)
          (ldb (byte 2 6) clock-seq-high) #b10 ; Set variant to IETF
          (ldb (byte 6 0) clock-seq-high) (ldb (byte 6 0) clock-seq))
    (make-instance 'uuid
                   ;; Set the timestamp
                   :time-low (ldb (byte 32 16) timestamp)
                   :time-mid (ldb (byte 16 0) timestamp)
                   ;; Contains version and high 12 bits from clock-seq
                   :time-hi-and-version time-high-and-version
                   ;; Contains variant, low 6 bits of clock-seq
                   :clock-seq-hi-and-res clock-seq-high
                   :clock-seq-low (ldb (byte 8 48) random)
                   :node (ldb (byte 48 0) random))))

(declaim (ftype (function () (values uuid &optional)) make-v7))
(defun make-v7 ()
  "Generate uuid value (version 7)."
  (unless *v7-generator* (initialize-v7-generator))
  (multiple-value-bind (base fraction repetitions)
      (funcall (v7-timestamp-generator *v7-generator*))
    ;; Change clock-seq when necessary
    (when (or
           ;; Time went backwards
           (null repetitions)
           ;; Ran out of unique values
           (and (plusp repetitions)
                (or fraction
                    (zerop (mod repetitions +millis-per-second+)))))
      (setf (v7-clock-seq *v7-generator*)
            (mod (1+ (v7-clock-seq *v7-generator*)) +v7-clock-seq-max+)))
    (make-v7-from-timestamp
     (+ (* base +millis-per-second+)
        (if fraction
            (floor fraction +nanos-per-milli+)
            (mod repetitions +millis-per-second+)))
     (random-integer #xFFFFFFFFFFFFFF))))