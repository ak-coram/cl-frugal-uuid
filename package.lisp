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
           #:make-v4))
