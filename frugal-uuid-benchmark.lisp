;;;; frugal-uuid-benchmark.lisp

(trivial-benchmark:define-benchmark-package #:frugal-uuid/benchmark
  (:export #:run-benchmarks))

(in-package #:frugal-uuid/benchmark)

(define-benchmark gen-100k-v1-uuids ()
  (declare (optimize speed))
  (fuuid:with-random (#'random (make-random-state t))
    (fuuid:with-v1-generator (fuuid:make-v1-generator)
      (dotimes (_ 10)
        (with-benchmark-sampling
          (dotimes (_ 100000) (fuuid:make-v1)))))))

(define-benchmark gen-100k-v4-uuids ()
  (declare (optimize speed))
  (fuuid:with-random (#'random (make-random-state t))
    (dotimes (_ 10)
      (with-benchmark-sampling
        (dotimes (_ 100000) (fuuid:make-v4))))))

(define-benchmark gen-100k-secure-random-v4-uuids ()
  (declare (optimize speed))
  (fuuid:with-random (#'secure-random:number secure-random:*generator*)
    (dotimes (_ 10)
      (with-benchmark-sampling
        (dotimes (_ 100000) (fuuid:make-v4))))))

(define-benchmark gen-100k-ironclad-random-v4-uuids ()
  (declare (optimize speed))
  (fuuid:with-random (#'crypto:strong-random (ironclad:make-prng :os))
    (dotimes (_ 10)
      (with-benchmark-sampling
        (dotimes (_ 100000) (fuuid:make-v4))))))

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
