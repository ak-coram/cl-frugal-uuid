;;;; frugal-uuid-v4.lisp

(in-package #:frugal-uuid)

(defvar *random-number-generator* nil)
(defvar *random-number-function* nil)

(defun initialize-v4-random (random-number-function
                             &optional random-number-generator)
  "Sets up alternate source of randomness for generating v4 UUIDs.

MAKE-V4 expects the provided RANDOM-NUMBER-FUNCTION to take two
arguments: the first is a limit below which a non-negative random
number should be generated and the second is the value of the
RANDOM-NUMBER-GENERATOR argument. When no random number generator is
provided, RANDOM-NUMBER-FUNCTION is called witha a single LIMIT
argument."
  (setf *random-number-generator* random-number-generator
        *random-number-function* random-number-function)
  nil)

(defun make-v4-from-integer (i)
  "Sets the bits for version 4 and IETF variant, returns uuid value."
  (setf (ldb (byte 4 76) i) #x4      ; Set version to random
        (ldb (byte 2 62) i) #b10)    ; Set variant to IETF
  (from-integer i))

(defun make-v4 (&optional random-number-generator)
  "Generate random uuid value (version 4).

When no alternative source of randomness is provided via
INITIALIZE-V4-RANDOM, then sets up its own
RANDOM-NUMBER-GENERATOR (via (MAKE-RANDOM-STATE T)) when first
invoked."
  (unless *random-number-function*
    (setf *random-number-generator* (make-random-state t)
          *random-number-function* #'random))
  ;; Generate 128-bit random value
  (let* ((limit #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
         (base (if (or random-number-generator
                       *random-number-generator*)
                   (funcall *random-number-function*
                            limit
                            (or random-number-generator
                                *random-number-generator*))
                   (funcall *random-number-function* limit))))
    (make-v4-from-integer base)))

(defmacro with-v4-random-number-generator (random-number-generator &body body)
  "Dynamically bind random number generator for creating uuid values."
  `(let ((*random-number-generator* ,random-number-generator))
     ,@body))

(defmacro with-v4-random ((random-number-function
                           &optional random-number-generator) &body body)
  "Dynamically bind source of randomness for creating uuid values."
  `(let ((*random-number-generator* ,random-number-generator)
         (*random-number-function* ,random-number-function))
     ,@body))


