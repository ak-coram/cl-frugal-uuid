;;;; frugal-uuid.lisp

(in-package #:frugal-uuid)

(defclass uuid ()
  ((time-low :initarg :time-low
             :accessor time-low
             :type (unsigned-byte 32)
             :documentation "Low 32 bits of the time")
   (time-mid :initarg :time-mid
             :accessor time-mid
             :type (unsigned-byte 16)
             :documentation "Middle 16 bits of the time")
   (time-hi-and-version
    :initarg :time-hi-and-version
    :accessor time-hi-and-version
    :type (unsigned-byte 16)
    :documentation "4-bit version followed by the high 12 bits of the time")
   (clock-seq-hi-and-res
    :initarg :clock-seq-hi-and-res
    :accessor clock-seq-hi-and-res
    :type (unsigned-byte 8)
    :documentation
    "1 to 3-bit variant followed by the 5 to 7 high bits of the clock sequence")
   (clock-seq-low :initarg :clock-seq-low
                  :accessor clock-seq-low
                  :type (unsigned-byte 8)
                  :documentation "Low 8 bits of the clock sequence")
   (node :initarg :node
         :accessor node
         :type (unsigned-byte 48)
         :documentation "48-bit node ID"))
  (:documentation "Represent UUID values as defined in RFC 4122"))

(declaim (ftype (function (integer) (values uuid &optional)) from-integer))
(defun from-integer (i)
  "Create uuid value from integer representation."
  (make-instance 'uuid
                 :time-low (ldb (byte 32 96) i)
                 :time-mid (ldb (byte 16 80) i)
                 :time-hi-and-version (ldb (byte 16 64) i)
                 :clock-seq-hi-and-res (ldb (byte 8 56) i)
                 :clock-seq-low (ldb (byte 8 48) i)
                 :node (ldb (byte 48 0) i)))

(declaim (ftype (function (uuid) integer) to-integer))
(defun to-integer (uuid)
  "Convert uuid value to integer representation."
  (let ((i 0))
    (setf (ldb (byte 32 96) i) (time-low uuid)
          (ldb (byte 16 80) i) (time-mid uuid)
          (ldb (byte 16 64) i) (time-hi-and-version uuid)
          (ldb (byte 8 56) i) (clock-seq-hi-and-res uuid)
          (ldb (byte 8 48) i) (clock-seq-low uuid)
          (ldb (byte 48 0) i) (node uuid))
    i))

(declaim (ftype (function (string) uuid) from-string))
(defun from-string (s)
  "Parse uuid value from canonical textual representation."
  (unless (eql (length s) 36)
    (error "UUID parse error: expected input string of length 36."))
  (loop
    :for i :in '(8 13 18 23)
    :for c := (aref s i)
    :unless (eql c #\-)
      :do (error "UUID parse error: expected - at index ~a, found ~a instead." i c))
  (from-integer (parse-integer (remove #\- s) :radix 16)))

(declaim (ftype (function (uuid) string) to-string))
(defun to-string (uuid)
  "Convert uuid value into canonical textual representation."
  (format nil "~(~8,'0x-~4,'0x-~4,'0x-~2,'0x~2,'0x-~12,'0x~)"
          (time-low uuid)
          (time-mid uuid)
          (time-hi-and-version uuid)
          (clock-seq-hi-and-res uuid)
          (clock-seq-low uuid)
          (node uuid)))

(declaim (ftype (function (integer) (simple-array (unsigned-byte 8)))
                integer-to-octets))
(defun integer-to-octets (i)
  (loop :with octets := (make-array '(16) :element-type '(unsigned-byte 8))
        :for octet-index :below 16
        :for bit-index := (* (- 15 octet-index) 8)
        :do (setf (aref octets octet-index)
                  (ldb (byte 8 bit-index) i))
        :finally (return octets)))

(declaim (ftype (function ((simple-array (unsigned-byte 8))) integer)
                octets-to-integer))
(defun octets-to-integer (octets)
  (loop :with i := 0
        :for octet-index :below 16
        :for bit-index := (* (- 15 octet-index) 8)
        :do (setf (ldb (byte 8 bit-index) i)
                  (aref octets octet-index))
        :finally (return i)))

(declaim (ftype (function (uuid) (simple-array (unsigned-byte 8))) to-octets))
(defun to-octets (uuid)
  "Create a vector of octets from uuid value."
  (integer-to-octets (to-integer uuid)))

(declaim (ftype (function ((simple-array (unsigned-byte 8))) uuid) from-octets))
(defun from-octets (octets)
  "Create uuid value from a vector of octets."
  (from-integer (octets-to-integer octets)))

(defmethod print-object ((uuid uuid) stream)
  (print-unreadable-object (uuid stream :type t)
    (format stream (to-string uuid))))

(declaim (ftype (function () (values uuid &optional)) make-nil))
(defun make-nil ()
  "Create uuid value with all bits set to zero."
  (make-instance 'uuid
                 :time-low 0
                 :time-mid 0
                 :time-hi-and-version 0
                 :clock-seq-hi-and-res 0
                 :clock-seq-low 0
                 :node 0))

(declaim (ftype (function () (values uuid &optional)) make-omni))
(defun make-omni ()
  "Create uuid value with all bits set to one."
  (make-instance 'uuid
                 :time-low #xFFFFFFFF
                 :time-mid #xFFFF
                 :time-hi-and-version #xFFFF
                 :clock-seq-hi-and-res #xFF
                 :clock-seq-low #xFF
                 :node #xFFFFFFFFFFFF))

(declaim (ftype (function (uuid uuid) boolean) uuid=))
(defun uuid= (x y)
  "Strictly compare uuid inputs for equality.

Only accepts inputs of type uuid."
  (or (eq x y)
      (and (eql (time-low x) (time-low y))
           (eql (time-mid x) (time-mid y))
           (eql (time-hi-and-version x) (time-hi-and-version y))
           (eql (clock-seq-hi-and-res x) (clock-seq-hi-and-res y))
           (eql (clock-seq-low x) (clock-seq-low y))
           (eql (node x) (node y)))))

(defun uuid-equal-p (x y)
  "Loosely compares inputs representing UUIDs.

In addition to values of type uuid, it accepts the canonical string,
an integer representation of UUIDs or a vector of octets."
  (or (eq x y)
      (and x y
           (let ((x (typecase x
                      (string (from-string x))
                      (integer (from-integer x))
                      ((simple-array (unsigned-byte 8)) (from-octets x))
                      (t x)))
                 (y (typecase y
                      (string (from-string y))
                      (integer (from-integer y))
                      ((simple-array (unsigned-byte 8)) (from-octets y))
                      (t y))))
             (uuid= x y)))))

(defun compile-literal (uuid-literal)
  (let ((uuid (typecase uuid-literal
                (uuid uuid-literal)
                (string (from-string uuid-literal))
                (integer (from-integer uuid-literal))
                ((simple-array (unsigned-byte 8)) (from-octets uuid-literal))
                (t (from-string (format nil "~a" uuid-literal))))))
    `(make-instance 'uuid
                    :time-low ,(time-low uuid)
                    :time-mid ,(time-mid uuid)
                    :time-hi-and-version ,(time-hi-and-version uuid)
                    :clock-seq-hi-and-res ,(clock-seq-hi-and-res uuid)
                    :clock-seq-low ,(clock-seq-low uuid)
                    :node ,(node uuid))))

(defmacro ~ (&rest uuid-literals)
  (if (null uuid-literals)
      (compile-literal (make-v4))
      `(values
        ,@(loop :for uuid-literal :in uuid-literals
                :collect (compile-literal uuid-literal)))))


