;;;; frugal-uuid-random.lisp

(in-package #:frugal-uuid)

(defvar *random-number-generator* nil)
(defvar *random-number-function* nil)

(defun initialize-random (random-number-function
                          &optional random-number-generator)
  "Sets up alternate source of randomness for generating UUIDs.

The provided RANDOM-NUMBER-FUNCTION is expected to take two arguments:
the first is a limit below which a non-negative random number should
be generated and the second is the value of the
RANDOM-NUMBER-GENERATOR argument. When no random number generator is
provided, RANDOM-NUMBER-FUNCTION is called with a single LIMIT
argument."
  (setf *random-number-generator* random-number-generator
        *random-number-function* random-number-function)
  nil)

(defmacro with-random-number-generator (random-number-generator &body body)
  "Dynamically bind random number generator for creating uuid values."
  `(let ((*random-number-generator* ,random-number-generator))
     ,@body))

(defmacro with-random ((random-number-function
                        &optional random-number-generator) &body body)
  "Dynamically bind source of randomness for creating uuid values."
  `(let ((*random-number-generator* ,random-number-generator)
         (*random-number-function* ,random-number-function))
     ,@body))

(declaim (ftype (function (integer) (values integer &optional))
                random-integer))
(defun random-integer (limit)
  "Generate a non-negative random number below LIMIT.

When no alternative source of randomness is provided via
INITIALIZE-RANDOM, then sets up its own random number
generator (via (MAKE-RANDOM-STATE T)) when first invoked."
  (unless *random-number-function*
    (setf *random-number-generator* (make-random-state t)
          *random-number-function* #'random))
  (if *random-number-generator*
      (funcall *random-number-function*
               limit
               *random-number-generator*)
      (funcall *random-number-function* limit)))
