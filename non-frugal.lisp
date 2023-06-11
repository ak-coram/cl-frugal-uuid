;;;; non-frugal.lisp

(in-package #:frugal-uuid)

;; Use Ironclad for generating random numbers
(initialize-random #'crypto:strong-random (ironclad:make-prng :os))
;; (Re)initialize version 1 generator
(initialize-v1-generator)

;; Setup new PRNG for each new thread
#+thread-support
(pushnew '(*random-number-generator* . (ironclad:make-prng :os))
         bt2:*default-special-bindings*
         :test #'equal)
#+thread-support
(pushnew '(*random-number-function* . #'crypto:strong-random)
         bt2:*default-special-bindings*
         :test #'equal)

;; Setup new version 1 generator for each new thread
#+thread-support
(pushnew '(*v1-generator* . (make-v1-generator))
         bt2:*default-special-bindings*
         :test #'equal)

(declaim (ftype (function (uuid string) uuid) make-v3))
(defun make-v3 (namespace name)
  (let ((digest (ironclad:make-digest :md5)))
    (ironclad:update-digest digest (fuuid:to-octets namespace))
    (ironclad:update-digest digest 
                            (babel:string-to-octets name :encoding :utf-8))
    (fuuid:make-v3-from-octets (ironclad:produce-digest digest))))

(declaim (ftype (function (uuid string) uuid) make-v5))
(defun make-v5 (namespace name)
    (let ((digest (ironclad:make-digest :sha1)))
      (ironclad:update-digest digest (fuuid:to-octets namespace))
      (ironclad:update-digest digest 
                              (babel:string-to-octets name :encoding :utf-8))
      (fuuid:make-v5-from-octets (ironclad:produce-digest digest))))
