;;;; frugal-uuid.asd

(asdf:defsystem #:frugal-uuid
  :description "Common Lisp UUID library with zero dependencies"
  :author "Ákos Kiss <ak@coram.pub>"
  :license "MIT License"
  :serial t
  :components ((:file "package")
               (:file "frugal-uuid")
               (:file "frugal-uuid-node")
               (:file "frugal-uuid-clock")
               (:file "frugal-uuid-random")
               (:file "frugal-uuid-namespace")
               (:file "frugal-uuid-v1")
               (:file "frugal-uuid-v2")
               (:file "frugal-uuid-v3")
               (:file "frugal-uuid-v4")
               (:file "frugal-uuid-v5")
               (:file "frugal-uuid-v6")
               (:file "frugal-uuid-v7")
               (:file "frugal-uuid-v8"))
  :in-order-to ((test-op (test-op "frugal-uuid/test"))))

(asdf:defsystem #:frugal-uuid/test
  :depends-on (#:frugal-uuid
               #:fiveam)
  :components ((:file "frugal-uuid-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :frugal-uuid)))

(asdf:defsystem #:frugal-uuid/non-frugal/strong-random
  :depends-on (#:frugal-uuid
               #:ironclad/prngs)
  :components ((:file "non-frugal/strong-random")))

(asdf:defsystem #:frugal-uuid/non-frugal/thread-safe
  :depends-on (#:frugal-uuid
               #:bordeaux-threads)
  :components ((:file "non-frugal/thread-safe")))

(asdf:defsystem #:frugal-uuid/non-frugal/name-based
  :depends-on (#:frugal-uuid
               #:babel
               #:ironclad/digest/md5
               #:ironclad/digest/sha1)
  :components ((:file "non-frugal/name-based")))

(asdf:defsystem #:frugal-uuid/non-frugal/accurate-clock
  :depends-on (#:frugal-uuid
               #:trivial-clock)
  :components ((:file "non-frugal/accurate-clock")))

(asdf:defsystem #:frugal-uuid/non-frugal/minara
  :depends-on (#:frugal-uuid
               #:frugal-uuid/non-frugal/accurate-clock
               #:frugal-uuid/non-frugal/strong-random)
  :components ((:file "non-frugal/minara")))

(asdf:defsystem #:frugal-uuid/non-frugal
  :depends-on (#:frugal-uuid/non-frugal/strong-random
               #:frugal-uuid/non-frugal/thread-safe
               #:frugal-uuid/non-frugal/name-based
               #:frugal-uuid/non-frugal/accurate-clock
               #:frugal-uuid/non-frugal/minara))

(asdf:defsystem #:frugal-uuid/benchmark
  :depends-on (#:frugal-uuid
               #:trivial-benchmark)
  :components ((:file "frugal-uuid-benchmark"))
  :perform (test-op
            (o c)
            (symbol-call :frugal-uuid/benchmark '#:run-benchmarks)))

(asdf:defsystem #:frugal-uuid/*
  :depends-on (#:frugal-uuid
               #:frugal-uuid/non-frugal
               #:frugal-uuid/benchmark
               #:frugal-uuid/test))
