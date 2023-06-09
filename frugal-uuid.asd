;;;; frugal-uuid.asd

(asdf:defsystem #:frugal-uuid
  :description "Common Lisp UUID library with zero dependencies"
  :author "Ákos Kiss <ak@coram.pub>"
  :license  "MIT License"
  :serial t
  :components ((:file "package")
               (:file "frugal-uuid")
               (:file "frugal-uuid-random")
               (:file "frugal-uuid-v4"))
  :in-order-to ((test-op (test-op "frugal-uuid/test"))))

(asdf:defsystem #:frugal-uuid/test
  :depends-on (#:frugal-uuid
               #:fiveam)
  :components ((:file "frugal-uuid-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :frugal-uuid)))

(asdf:defsystem #:frugal-uuid/*
  :depends-on (#:frugal-uuid
               #:frugal-uuid/test))
