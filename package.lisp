;;;; package.lisp

(defpackage #:frugal-uuid
  (:nicknames #:fuuid)
  (:use #:cl)
  (:export #:uuid
           #:from-integer
           #:to-integer
           #:from-string
           #:to-string
           #:make-nil
           #:uuid=
           #:uuid-equal-p

           ;; Randomness
           #:*random-number-generator*
           #:*random-number-function*
           #:initialize-random
           #:with-random
           #:with-random-number-generator

           ;; Clock
           #:random-clock-seq
           #:make-timestamp-generator

           ;; Node
           #:random-node-id
           #:parse-node-id

           ;; Version 1
           #:*v1-generator*
           #:make-v1-generator
           #:initialize-v1-generator
           #:with-v1-generator
           #:make-v1-from-timestamp
           #:make-v1

           ;; Version 4
           #:make-v4-from-integer
           #:make-v4))
