;;;; frugal-uuid-node.lisp

(in-package #:frugal-uuid)

(defun random-node-id ()
  "Generate random node ID with multicast bit set."
  (let ((i (random-integer #xFFFFFFFFFFFF)))
    (setf (ldb (byte 1 40) i) #b1)
    i))

(defun parse-node-id (mac-address)
  (unless (eql (length mac-address) 17)
    (error "MAC address parse error: expected input string of length 17."))
  (loop :for i :from 2 :by 3 :upto 14
    :for c := (aref mac-address i)
    :unless (eql c #\:)
      :do (error "MAC address parse error: expected : at index ~a, found ~a instead." i c))
  (parse-integer (remove #\: mac-address) :radix 16))


