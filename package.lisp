;;;; package.lisp

(defpackage #:frugal-uuid
  (:nicknames #:fuuid)
  (:use #:cl)
  (:export #:uuid
           #:time-low
           #:time-mid
           #:time-hi-and-version
           #:clock-seq-hi-and-res
           #:clock-seq-low
           #:node
           #:from-integer
           #:to-integer
           #:from-string
           #:to-string
           #:from-octets
           #:to-octets
           #:from-sym
           #:to-sym
           #:make-nil
           #:make-omni
           #:uuid=
           #:uuid-equal-p
           #:compile-literal
           #:~

           ;; Randomness
           #:*random-number-generator*
           #:*random-number-generator-init-function*
           #:*random-number-function*
           #:initialize-random
           #:with-random
           #:with-random-number-generator
           #:random-integer

           ;; Clock
           #:*unix-timestamp-function*
           #:random-clock-seq
           #:make-timestamp-generator

           ;; Node
           #:random-node-id
           #:parse-node-id

           ;; Namespace
           #:*ns-url*
           #:*ns-dns*
           #:*ns-oid*
           #:*ns-x500*

           ;; Version 1
           #:*v1-generator*
           #:*v1-generator-init-function*
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
           #:make-v5

           ;; Version 6 (IETF draft)
           #:v6-time-hi
           #:v6-time-mid
           #:v6-time-low-and-version
           #:make-v6-from-v1
           #:make-v6

           ;; Version 7 (IETF draft)
           #:*v7-generator*
           #:*v7-generator-init-function*
           #:make-v7-generator
           #:initialize-v7-generator
           #:with-v7-generator
           #:make-v7-from-timestamp
           #:make-v7

           ;; Version 8 (IETF draft)
           #:make-v8

           ;; MiNaRa (custom timestamp-based UUID via version 8, not
           ;; implemented in the base frugal system)
           #:+minara-max-random+
           #:minara-components
           #:make-minara-from-components
           #:make-minara
           #:*minara-min*
           #:*minara-max*))
