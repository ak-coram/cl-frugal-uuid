;;;; frugal-uuid-clock.lisp

(in-package #:frugal-uuid)

(defconstant +millis-per-second+ 1000)
(defconstant +nanos-per-second+ 1000000000)
(defconstant +nanos-per-milli+ 1000000)
(defconstant +100nanos-per-second+ (/ +nanos-per-second+ 100))

(defconstant +unix-time-uuid-epoch-offset-seconds+ 12219292800)
(defconstant +universal-time-unix-epoch-offset+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun get-unix-time ()
  (values (- (get-universal-time)
             +universal-time-unix-epoch-offset+)
          nil))

(defvar *unix-timestamp-function* #'get-unix-time
  "Function should return the number of seconds since the unix epoch and
the number of additional nanoseconds as a second value or NIL if
unknown.")

(defun random-clock-seq ()
  (random-integer #b11111111111111))

(defun get-current-timestamp (uuid-epoch low-resolution)
  (multiple-value-bind (seconds nanos) (funcall *unix-timestamp-function*)
    (let* ((seconds (if uuid-epoch
                        (+ seconds +unix-time-uuid-epoch-offset-seconds+)
                        seconds))
           (nanos (if (and nanos low-resolution)
                      (floor nanos 100)
                      nanos)))
      (values seconds nanos))))

(defun make-timestamp-generator
    (&key (uuid-epoch t) (low-resolution t) (repetitions-increment 1))
  (let ((previous-base nil)
        (previous-fraction nil)
        (repetitions 0))
    (lambda ()
      (multiple-value-bind (base fraction)
          (get-current-timestamp uuid-epoch low-resolution)
        (labels ((return-results (n)
                   (setf previous-base base
                         previous-fraction fraction)
                   (values base fraction n)))
          (cond
            ;; No previous timestamp means no repetitions
            ((null previous-base) (return-results repetitions))

            ;; Timestamp is the same as the previous one
            ((and (eql base previous-base)
                  (eql fraction previous-fraction))
             (return-results (incf repetitions repetitions-increment)))

            ;; Time went backwards, unknown number of repetitions
            ((or (< base previous-base)
                 (and (eql base previous-base)
                      fraction
                      (< fraction previous-fraction)))
             (setf repetitions 0)
             (return-results nil))

            ;; We have a new timestamp, reset number of repetitions
            (t (setf repetitions 0)
               (return-results repetitions))))))))
