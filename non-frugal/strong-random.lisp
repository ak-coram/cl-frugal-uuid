;;;; non-frugal/strong-random.lisp

(in-package #:frugal-uuid)

;; Use Ironclad for generating random numbers
(initialize-random #'crypto:strong-random (ironclad:make-prng :os))
;; (Re)initialize version 1 generator
(initialize-v1-generator)
