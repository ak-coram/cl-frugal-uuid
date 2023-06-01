;;;; frugal-uuid.lisp

(in-package #:frugal-uuid)

(defclass uuid ()
  ((time-low :initarg :time-low
             :accessor time-low
             :type (unsigned-byte 32))
   (time-mid :initarg :time-mid
             :accessor time-mid
             :type (unsigned-byte 16))
   (time-hi-and-version :initarg :time-hi-and-version
                        :accessor time-hi-and-version
                        :type (unsigned-byte 16))
   (clock-seq-hi-and-res :initarg :clock-seq-hi-and-res
                         :accessor clock-seq-hi-and-res
                         :type (unsigned-byte 8))
   (clock-seq-low :initarg :clock-seq-low
                  :accessor clock-seq-low
                  :type (unsigned-byte 8))
   (node :initarg :node
         :accessor node
         :type (unsigned-byte 48))))

(defun from-integer (i)
  (make-instance 'uuid
                 :time-low (ldb (byte 32 96) i)
                 :time-mid (ldb (byte 16 80) i)
                 :time-hi-and-version (ldb (byte 16 64) i)
                 :clock-seq-hi-and-res (ldb (byte 8 56) i)
                 :clock-seq-low (ldb (byte 8 48) i)
                 :node (ldb (byte 48 0) i)))

(defun to-integer (uuid)
  (let ((i 0))
    (setf (ldb (byte 32 96) i) (time-low uuid)
          (ldb (byte 16 80) i) (time-mid uuid)
          (ldb (byte 16 64) i) (time-hi-and-version uuid)
          (ldb (byte 8 56) i) (clock-seq-hi-and-res uuid)
          (ldb (byte 8 48) i) (clock-seq-low uuid)
          (ldb (byte 48 0) i) (node uuid))
    i))

(defun from-string (s)
  (unless (eql (length s) 36)
    (error "UUID parse error: expected input string of length 36."))
  (loop
    :for i :in '(8 13 18 23)
    :for c := (aref s i)
    :unless (eql c #\-)
      :do (error "UUID parse error: expected - at index ~a, found ~a instead." i c))
  (from-integer (parse-integer (remove #\- s) :radix 16)))

(defun to-string (uuid)
  (format nil "~(~8,'0x-~4,'0x-~4,'0x-~2,'0x~2,'0x-~12,'0x~)"
          (time-low uuid)
          (time-mid uuid)
          (time-hi-and-version uuid)
          (clock-seq-hi-and-res uuid)
          (clock-seq-low uuid)
          (node uuid)))

(defmethod print-object ((uuid uuid) stream)
  (print-unreadable-object (uuid stream :type t)
    (format stream (to-string uuid))))

(defun make-nil ()
  (make-instance 'uuid
                 :time-low 0
                 :time-mid 0
                 :time-hi-and-version 0
                 :clock-seq-hi-and-res 0
                 :clock-seq-low 0
                 :node 0))

(declaim (ftype (function (uuid uuid) boolean) uuid=))
(defun uuid= (x y)
  (or (eq x y)
      (and (eql (time-low x) (time-low y))
           (eql (time-mid x) (time-mid y))
           (eql (time-hi-and-version x) (time-hi-and-version y))
           (eql (clock-seq-hi-and-res x) (clock-seq-hi-and-res y))
           (eql (clock-seq-low x) (clock-seq-low y))
           (eql (node x) (node y)))))

(defun uuid-equal-p (x y)
  (or (eq x y)
      (and x y
           (let ((x (if (stringp x)
                        (from-string x)
                        x))
                 (y (if (stringp y)
                        (from-string y)
                        y)))
             (uuid= x y)))))
