;;;; frugal-uuid-test.lisp

(defpackage #:frugal-uuid-test
  (:use #:cl #:fiveam))

(in-package #:frugal-uuid-test)

(def-suite :frugal-uuid)
(in-suite :frugal-uuid)

(test integer-conversion
  (is (fuuid:uuid= (fuuid:make-nil) (fuuid:from-integer 0)))
  (is (eql 0 (fuuid:to-integer (fuuid:make-nil))))
  (dotimes (_ 20)
    (let* ((uuid (fuuid:make-v4))
           (i (fuuid:to-integer uuid)))
      (is (integerp i))
      (is (fuuid:uuid= uuid (fuuid:from-integer i))))))

(test string-conversion
  (is (string= "00000000-0000-0000-0000-000000000000"
               (fuuid:to-string (fuuid:make-nil))))
  (is (fuuid:uuid= (fuuid:make-nil)
                   (fuuid:from-string "00000000-0000-0000-0000-000000000000")))
  (dotimes (_ 20)
    (let* ((uuid (fuuid:make-v4))
           (s (fuuid:to-string uuid)))
      (is (stringp s))
      (is (fuuid:uuid= uuid (fuuid:from-string s))))))

(test octet-conversion
  (let ((nil-octets (make-array '(16)
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (is (equalp nil-octets (fuuid:to-octets (fuuid:make-nil))))
    (is (fuuid:uuid= (fuuid:make-nil)
                     (fuuid:from-octets nil-octets)))
    (dotimes (_ 20)
      (let* ((uuid (fuuid:make-v4))
             (octets (fuuid:to-octets uuid)))
        (is (vectorp octets))
        (is (fuuid:uuid= uuid (fuuid:from-octets octets)))))))

(test equality
  (is (fuuid:uuid= (fuuid:make-nil) (fuuid:make-nil)))
  (is (fuuid:uuid-equal-p nil nil))
  (is (not (fuuid:uuid-equal-p nil (fuuid:make-nil))))
  (is (not (fuuid:uuid= (fuuid:make-v1) (fuuid:make-v4))))
  (dotimes (_ 5)
    (let* ((uuid (fuuid:make-v4))
           (fns `((,#'identity . ,#'identity)
                  (,#'fuuid:to-integer . ,#'fuuid:from-integer)
                  (,#'fuuid:to-string . ,#'fuuid:from-string)
                  (,#'fuuid:to-octets . ,#'fuuid:from-octets)
                  (,#'fuuid:to-sym . ,#'fuuid:from-sym)))
           (values (loop :with values := nil
                         :for (to-fn . from-fn) :in fns
                         :for v := (funcall to-fn uuid)
                         :do (push v values)
                         :do (push (funcall from-fn v) values)
                         :finally (return values))))
      (is (eql (* (length values)
                  (length values))
               (loop :for a :in values
                     :sum (loop :for b :in values
                                :count (and (fuuid:uuid-equal-p a b)
                                            (fuuid:uuid-equal-p b a))))))))
  (is (loop :with uuids := '("00000000-0000-0000-0000-000000000000"
                             "ef4c23eb-1fc0-4216-981d-9e24d512d9f4"
                             "3dbbd860-a35c-47df-8952-7604398ad84c"
                             "9215d239-4d04-4e1b-8dda-61e647bc2fc7"
                             "a4cb7801-d568-47b6-a2bf-e3b7e0770e76"
                             "0b817bd9-58e1-4352-93ca-549e0c91024f"
                             "f9e035f7-d2ea-46a4-827f-7cd19961fa3c"
                             "0ce58808-9db1-447a-a2ac-241b413ad409"
                             "dce81bfb-b6ce-4982-9ed3-e0010c21a8b9"
                             "67e64cf2-ad4a-4661-9b5d-e8c51d54c913"
                             "023b9bc2-3c24-4b8d-8294-2ac0858dff71")
            :for a :in uuids
            :count (loop :for b :in uuids
                         :count (or (fuuid:uuid-equal-p a b)
                                    (fuuid:uuid= (fuuid:from-string a)
                                                 (fuuid:from-string b))))
              :into n
            :finally (return (eql (length uuids) n)))))

(test v1-uniqueness
  (is (zerop
       (loop :with generator
               := (fuuid:make-v1-generator
                   :timestamp-generator (fuuid:make-timestamp-generator
                                         :repetitions-increment 1000000))
             :with prev := nil
             :for x :below 30
             :for id := (fuuid:with-v1-generator generator
                          (fuuid:to-integer (fuuid:make-v1)))
             :count (eql id prev)
             :do (setf prev id)))))
