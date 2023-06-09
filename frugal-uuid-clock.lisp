;;;; frugal-uuid-clock.lisp

(in-package #:frugal-uuid)

(defun random-clock-seq ()
  (random-integer #b11111111111111))

(defun make-timestamp-generator
    (&key (sleep-interval 0.01) (offset-increment 1))
  "Create up to 10000000 unique timestamp values per second.

Sleeps when unique values are exhausted, SLEEP-INTERVAL can be used to
determine in what increments sleep is called."
  (let ((previous-base nil)
        (offset 0))
    (lambda ()
      (block nil
        (tagbody again
           (let* ((n 10000000)
                  (base (+ (get-universal-time) 10010304000))
                  (timestamp (* base n)))
             (if (eql base previous-base)
                 (if (< offset (- n offset-increment))
                     (return (+ timestamp (incf offset offset-increment)))
                     (progn
                       (sleep sleep-interval)
                       (go again)))
                 (progn
                   (setf previous-base base
                         offset 0)
                   (return timestamp)))))))))
