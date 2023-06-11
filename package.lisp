;;;; package.lisp

(defpackage #:frugal-uuid
  (:nicknames #:fuuid)
  (:use #:cl)
  (:export #:uuid
           #:from-integer
           #:to-integer
           #:from-string
           #:to-string
           #:from-octets
           #:to-octets
           #:make-nil
           #:make-omni
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

           ;; Namespace
           #:+ns-url+
           #:+ns-dns+
           #:+ns-oid+
           #:+ns-x500+
           #:*ns-url*
           #:*ns-dns*
           #:*ns-oid*
           #:*ns-x500*

           ;; Version 1
           #:*v1-generator*
           #:make-v1-generator
           #:initialize-v1-generator
           #:with-v1-generator
           #:make-v1-from-timestamp
           #:make-v1

           ;; Version 2 (not implemented)

           ;; Version 3
           #:make-v3-from-integer
           #:make-v3-from-octets
           #:make-v3

           ;; Version 4
           #:make-v4-from-integer
           #:make-v4

           ;; Version 5
           #:make-v5-from-integer
           #:make-v5-from-octets
           #:make-v5))
