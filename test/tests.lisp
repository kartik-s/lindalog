;;;; tests.lisp

(in-package :lindalog/tests)

(def-suite lindalog)
(in-suite lindalog)

(test constant-construction
  (let ((c (lindalog:make-constant :value 'x)))
    (is (eql 'x
             (lindalog:constant-value c)))))

(test constants-match-fail
  (let ((a (lindalog:make-constant :value 'a))
        (b (lindalog:make-constant :value 'b)))
    (is (eql lindalog::+match-fail+
             (lindalog::match-term a b lindalog::+no-bindings+)))))

(test constants-match-success
  (let ((a (lindalog:make-constant :value 3))
        (b (lindalog:make-constant :value 3)))
    (is (eql lindalog::+no-bindings+
             (lindalog::match-term a b lindalog::+no-bindings+)))))
