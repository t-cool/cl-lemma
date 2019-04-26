(defpackage lemmatizer/tests/main
  (:use :cl
        :lemmatizer
        :rove))
(in-package :lemmatizer/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lemmatizer)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
