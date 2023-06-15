;;;; frugal-uuid-random.lisp

(in-package #:frugal-uuid)

(defvar *random-number-generator* nil)
(defvar *random-number-generator-init-function*
  (lambda () (make-random-state t)))
(defvar *random-number-function* #'random)

(defun initialize-random (random-fn &optional init-fn)
  "Sets up alternate source of randomness for generating UUIDs.

The provided RANDOM-FN is expected to take two arguments: the first is
a limit below which a non-negative random number should be generated
and the second is the value returned by the INIT-FN function
argument. When no random INIT-FN is provided, RANDOM-FN is called with
a single LIMIT argument."
  (setf *random-number-generator* nil
        *random-number-generator-init-function* init-fn
        *random-number-function* random-fn)
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
INITIALIZE-RANDOM, then sets up its own random number generator via
*RANDOM-NUMBER-GENERATOR-INIT-FUNCTION* when first invoked."
  (when (and (not *random-number-generator*)
             *random-number-generator-init-function*)
    (setf *random-number-generator*
          (funcall *random-number-generator-init-function*)))
  (if *random-number-generator*
      (funcall *random-number-function*
               limit
               *random-number-generator*)
      (funcall *random-number-function* limit)))
