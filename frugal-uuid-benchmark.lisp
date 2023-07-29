;;;; frugal-uuid-benchmark.lisp

(trivial-benchmark:define-benchmark-package #:frugal-uuid/benchmark
  (:export #:run-benchmarks))

(in-package #:frugal-uuid/benchmark)

(define-benchmark gen-100k-v1-uuids ()
  (declare (optimize speed))
  (dotimes (_ 10)
    (with-benchmark-sampling
      (dotimes (_ 100000) (fuuid:make-v1)))))

(when (fboundp 'fuuid:make-v3)
  (define-benchmark gen-100k-v3-uuids ()
    (declare (optimize speed))
    (dotimes (_ 10)
      (with-benchmark-sampling
        (dotimes (_ 100000) (fuuid:make-v3 fuuid:*ns-url*
                                           "https://html5zombo.com/"))))))

(define-benchmark gen-100k-v4-uuids ()
  (declare (optimize speed))
  (dotimes (_ 10)
    (with-benchmark-sampling
      (dotimes (_ 100000) (fuuid:make-v4)))))

(when (fboundp 'fuuid:make-v5)
  (define-benchmark gen-100k-v5-uuids ()
    (declare (optimize speed))
    (dotimes (_ 10)
      (with-benchmark-sampling
        (dotimes (_ 100000) (fuuid:make-v5 fuuid:*ns-url*
                                           "https://html5zombo.com/"))))))

(define-benchmark gen-100k-v6-uuids ()
  (declare (optimize speed))
  (dotimes (_ 10)
    (with-benchmark-sampling
      (dotimes (_ 100000) (fuuid:make-v6)))))

(define-benchmark gen-100k-v7-uuids ()
  (declare (optimize speed))
  (dotimes (_ 10)
    (with-benchmark-sampling
      (dotimes (_ 100000) (fuuid:make-v7)))))

(when (fboundp 'fuuid:make-minara)
  (define-benchmark gen-100k-minara-uuids ()
    (declare (optimize speed))
    (dotimes (_ 10)
      (with-benchmark-sampling
        (dotimes (_ 100000) (fuuid:make-minara))))))

(defun floatify-results (benchmark-results)
  (loop :for v :being :each :hash-values :of benchmark-results
          :using (hash-key k)
        :do (setf (gethash k benchmark-results)
                  (loop :for metric :in v
                        :collect (loop :for n :in metric
                                       :collect (typecase n
                                                  (ratio (float n))
                                                  (t n)))))
        :finally (return benchmark-results)))

(defun run-benchmarks ()
  (let ((results (run-package-benchmarks :package '#:frugal-uuid/benchmark
                                         :verbose nil)))
    (report (floatify-results results))))
