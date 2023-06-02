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

           ;; Version 4
           #:initialize-v4-random
           #:make-v4-from-integer
           #:make-v4
           #:with-v4-random
           #:with-v4-random-number-generator))
