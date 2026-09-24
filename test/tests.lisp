;;;; tests.lisp

(in-package :lindalog/tests)

(def-suite lindalog)
(in-suite lindalog)

(test constant-construction-symbol
  (let ((c (lindalog:make-constant 'x)))
    (is (lindalog:constant-p c))
    (is (eql 'x (lindalog:constant-value c)))))

(test constant-construction-number
  (let ((c (lindalog:make-constant 4)))
    (is (lindalog:constant-p c))
    (is (eql 4 (lindalog:constant-value c)))))

(test constants-match-fail
  (let ((a (lindalog:make-constant 'a))
        (b (lindalog:make-constant 'b)))
    (is (eq lindalog::+match-fail+
            (lindalog::match-term a b lindalog::+no-bindings+)))))

(test constants-match-success
  (let ((a (lindalog:make-constant 3))
        (b (lindalog:make-constant 3)))
    (is (eq lindalog::+no-bindings+
            (lindalog::match-term a b lindalog::+no-bindings+)))))

(test variable-construction
  (let ((v (lindalog:make-variable 'x)))
    (is (lindalog:variable-p v))
    (is (eq 'x (lindalog:variable-name v)))))

(test variable-match-number
  (let* ((var (lindalog:make-variable 'at))
         (const (lindalog:make-constant 4))
         (bindings (lindalog::match-term var const lindalog::+no-bindings+)))
    (is (lindalog:ast-equal-p const (lindalog::lookup var bindings)))))

(test variable-match-symbol
  (let* ((var (lindalog:make-variable 'location))
         (const (lindalog:make-constant 'gate))
         (bindings (lindalog::match-term var const lindalog::+no-bindings+)))
    (is (lindalog:ast-equal-p const (lindalog::lookup var bindings)))))

(test bound-variable-match-success
  (let* ((var (lindalog:make-variable 'x))
         (bound-const (lindalog:make-constant 4))
         (const (lindalog:make-constant 4))
         (bindings (list (cons var bound-const))))
    (is (lindalog::bindings-equal-p
         bindings
         (lindalog::match-term var const bindings)))))

(test bound-variable-match-fail
  (let* ((var (lindalog:make-variable 'x))
         (bound-const (lindalog:make-constant 4))
         (const (lindalog:make-constant 3))
         (bindings (list (cons var bound-const))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-term var const bindings)))))

(test atom-construction
  (let ((a (lindalog:make-atom
            'at
            (list (lindalog:make-constant 'player)
                  (lindalog:make-constant 'gate)))))
    (is (lindalog:atom-p a))
    (is (eq 'at (lindalog:atom-predicate a)))
    (is (eq 'player (lindalog:constant-value (first (lindalog:atom-args a)))))
    (is (eq 'gate (lindalog:constant-value (second (lindalog:atom-args a)))))))
