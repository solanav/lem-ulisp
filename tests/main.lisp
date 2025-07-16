(defpackage pico-repl/tests/main
  (:use :cl
        :pico-repl
        :rove))
(in-package :pico-repl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :pico-repl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
