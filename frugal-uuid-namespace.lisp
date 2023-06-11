;;;; frugal-uuid-namespace.lisp

(in-package #:frugal-uuid)

(defmacro def-namespace-constant (sym s)
  `(defconstant ,sym ,(to-integer (from-string s))))

(def-namespace-constant +ns-url+ "6ba7b811-9dad-11d1-80b4-00c04fd430c8")
(def-namespace-constant +ns-dns+ "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
(def-namespace-constant +ns-oid+ "6ba7b812-9dad-11d1-80b4-00c04fd430c8")
(def-namespace-constant +ns-x500+ "6ba7b814-9dad-11d1-80b4-00c04fd430c8")

(defparameter *ns-url* (from-integer +ns-url+))
(defparameter *ns-dns* (from-integer +ns-dns+))
(defparameter *ns-oid* (from-integer +ns-oid+))
(defparameter *ns-x500* (from-integer +ns-x500+))
