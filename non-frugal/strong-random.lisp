;;;; non-frugal/strong-random.lisp

(in-package #:frugal-uuid)

;; Use Ironclad for generating random numbers
(initialize-random #'crypto:strong-random
                   (lambda () (ironclad:make-prng :os)))

;; Clear version 1 & 7 generators (they will be initialized on first
;; use):
(setf *v1-generator* nil
      *v7-generator* nil)
