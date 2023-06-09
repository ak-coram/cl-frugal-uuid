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
           #:initialize-random
           #:with-random
           #:with-random-number-generator

           ;; Version 4
           #:make-v4-from-integer
           #:make-v4))
