;;;; non-frugal/name-based.lisp

(in-package #:frugal-uuid)

(declaim (ftype (function (uuid string) (values uuid &optional)) make-v3))
(defun make-v3 (namespace name)
  (let ((digest (ironclad:make-digest :md5)))
    (ironclad:update-digest digest (fuuid:to-octets namespace))
    (ironclad:update-digest digest 
                            (babel:string-to-octets name :encoding :utf-8))
    (fuuid:make-v3-from-octets (ironclad:produce-digest digest))))

(declaim (inline make-v3-integer))
(defun make-v3-integer (namespace name)
  (to-integer (make-v3 namespace name)))

(declaim (inline make-v3-string))
(defun make-v3-string (namespace name)
  (to-string (make-v3 namespace name)))

(declaim (inline make-v3-octets))
(defun make-v3-octets (namespace name)
  (to-octets (make-v3 namespace name)))

(declaim (inline make-v3-sym))
(defun make-v3-sym (namespace name)
  (to-sym (make-v3 namespace name)))

(declaim (ftype (function (uuid string) (values uuid &optional)) make-v5))
(defun make-v5 (namespace name)
    (let ((digest (ironclad:make-digest :sha1)))
      (ironclad:update-digest digest (fuuid:to-octets namespace))
      (ironclad:update-digest digest 
                              (babel:string-to-octets name :encoding :utf-8))
      (fuuid:make-v5-from-octets (ironclad:produce-digest digest))))

(declaim (inline make-v5-integer))
(defun make-v5-integer (namespace name)
  (to-integer (make-v5 namespace name)))

(declaim (inline make-v5-string))
(defun make-v5-string (namespace name)
  (to-string (make-v5 namespace name)))

(declaim (inline make-v5-octets))
(defun make-v5-octets (namespace name)
  (to-octets (make-v5 namespace name)))

(declaim (inline make-v5-sym))
(defun make-v5-sym (namespace name)
  (to-sym (make-v5 namespace name)))
