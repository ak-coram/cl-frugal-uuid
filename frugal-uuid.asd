;;;; frugal-uuid.asd

(asdf:defsystem #:frugal-uuid
  :description "Common Lisp UUID library with zero dependencies"
  :author "Ákos Kiss <ak@coram.pub>"
  :license  "MIT License"
  :serial t
  :components ((:file "package")
               (:file "frugal-uuid")
               (:file "frugal-uuid-node")
               (:file "frugal-uuid-clock")
               (:file "frugal-uuid-random")
               (:file "frugal-uuid-v1")
               (:file "frugal-uuid-v4")
               (:file "frugal-uuid-v5"))
  :in-order-to ((test-op (test-op "frugal-uuid/test"))))

(asdf:defsystem #:frugal-uuid/test
  :depends-on (#:frugal-uuid
               #:fiveam)
  :components ((:file "frugal-uuid-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :frugal-uuid)))

(asdf:defsystem #:frugal-uuid/non-frugal
  :depends-on (#:frugal-uuid
               #:babel
               #:bordeaux-threads
               #:ironclad/prngs
               #:ironclad/digest/md5
               #:ironclad/digest/sha1)
  :components ((:file "non-frugal")))

(asdf:defsystem #:frugal-uuid/benchmark
  :depends-on (#:frugal-uuid
               #:trivial-benchmark
               #:ironclad/prngs
               #:secure-random)
  :components ((:file "frugal-uuid-benchmark"))
  :perform (test-op
            (o c)
            (symbol-call :frugal-uuid/benchmark '#:run-benchmarks)))

(asdf:defsystem #:frugal-uuid/*
  :depends-on (#:frugal-uuid
               #:frugal-uuid/test
               #:frugal-uuid/benchmark
               #:frugal-uuid/non-frugal))
