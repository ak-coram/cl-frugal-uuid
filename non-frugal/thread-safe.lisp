;;;; non-frugal/thread-safe.lisp

(in-package #:frugal-uuid)

;; Setup new PRNG for each new thread
#+thread-support
(pushnew '(*random-number-generator* .
           (funcall *random-number-generator-init-function*))
         bt2:*default-special-bindings*
         :test #'equal)

;; Setup new version 1 generator for each new thread
#+thread-support
(pushnew '(*v1-generator* . (make-v1-generator))
         bt2:*default-special-bindings*
         :test #'equal)

