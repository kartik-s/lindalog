;;;; tests.lisp

(in-package :lindalog/tests)

(def-suite lindalog)
(in-suite lindalog)

(test constant-with-symbol-value
  (let ((c (lindalog:make-constant 'x)))
    (is (lindalog:constant-p c))
    (is (eql 'x (lindalog:constant-value c)))))

(test constant-with-number-value
  (let ((c (lindalog:make-constant 4)))
    (is (lindalog:constant-p c))
    (is (eql 4 (lindalog:constant-value c)))))

(test match-term-unequal-constants-fails
  (let ((a (lindalog:make-constant 'a))
        (b (lindalog:make-constant 'b)))
    (is (eq lindalog::+match-fail+
            (lindalog::match-term a b lindalog::+no-bindings+)))))

(test match-term-equal-constants-succeeds
  (let ((a (lindalog:make-constant 3))
        (b (lindalog:make-constant 3)))
    (is (eq lindalog::+no-bindings+
            (lindalog::match-term a b lindalog::+no-bindings+)))))

(test variable-with-symbol-name
  (let ((v (lindalog:make-variable 'x)))
    (is (lindalog:variable-p v))
    (is (eq 'x (lindalog:variable-name v)))))

(test match-term-unbound-variable-binds-number
  (let* ((var (lindalog:make-variable 'at))
         (const (lindalog:make-constant 4))
         (bindings (lindalog::match-term var const lindalog::+no-bindings+)))
    (is (lindalog:ast-equal-p const (lindalog::lookup var bindings)))))

(test match-term-unbound-variable-binds-symbol
  (let* ((var (lindalog:make-variable 'location))
         (const (lindalog:make-constant 'gate))
         (bindings (lindalog::match-term var const lindalog::+no-bindings+)))
    (is (lindalog:ast-equal-p const (lindalog::lookup var bindings)))))

(test match-term-bound-variable-equal-value-succeeds
  (let* ((var (lindalog:make-variable 'x))
         (bound-const (lindalog:make-constant 4))
         (const (lindalog:make-constant 4))
         (bindings (list (cons var bound-const))))
    (is (lindalog::bindings-equal-p
         bindings
         (lindalog::match-term var const bindings)))))

(test match-term-bound-variable-unequal-value-fails
  (let* ((var (lindalog:make-variable 'x))
         (bound-const (lindalog:make-constant 4))
         (const (lindalog:make-constant 3))
         (bindings (list (cons var bound-const))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-term var const bindings)))))

(test match-term-unbound-variable-preserves-existing-bindings
  (let ((bindings (list (cons (lindalog:make-variable 'x)
                              (lindalog:make-constant 'a)))))
    (is (lindalog::bindings-equal-p
         (list (cons (lindalog:make-variable 'x)
                     (lindalog:make-constant 'a))
               (cons (lindalog:make-variable 'y)
                     (lindalog:make-constant 'b)))
         (lindalog::match-term (lindalog:make-variable 'y)
                               (lindalog:make-constant 'b)
                               bindings)))))

(test atom-with-predicate-and-arguments
  (let ((a (lindalog:make-atom
            'at
            (list (lindalog:make-constant 'player)
                  (lindalog:make-constant 'gate)))))
    (is (lindalog:atom-p a))
    (is (eq 'at (lindalog:atom-predicate a)))
    (is (eq 'player (lindalog:constant-value (first (lindalog:atom-args a)))))
    (is (eq 'gate (lindalog:constant-value (second (lindalog:atom-args a)))))))

(test match-atom-equal-ground-atoms-succeeds
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b)))))
    (is (eq lindalog::+no-bindings+
            (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-different-predicates-fails
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
        (b (lindalog:make-atom 'q (list (lindalog:make-constant 'a)))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-unequal-ground-arguments-fails
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'b)))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-different-arities-fails
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b)))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-unbound-variable-creates-binding
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))))
    (is (lindalog::bindings-equal-p
         (list (cons (lindalog:make-variable 'x)
                     (lindalog:make-constant 'a)))
         (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-multiple-variables-create-bindings
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x)
                                        (lindalog:make-variable 'y))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b)))))
    (is (lindalog::bindings-equal-p
         (list (cons (lindalog:make-variable 'x)
                     (lindalog:make-constant 'a))
               (cons (lindalog:make-variable 'y)
                     (lindalog:make-constant 'b)))
         (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-mixed-constant-and-variable-succeeds
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-variable 'x))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b)))))
    (is (lindalog::bindings-equal-p
         (list (cons (lindalog:make-variable 'x)
                     (lindalog:make-constant 'b)))
         (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-repeated-variable-consistent-succeeds
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x)
                                        (lindalog:make-variable 'x))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'a)))))
    (is (lindalog::bindings-equal-p
         (list (cons (lindalog:make-variable 'x)
                     (lindalog:make-constant 'a)))
         (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-repeated-variable-inconsistent-fails
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x)
                                        (lindalog:make-variable 'x))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b)))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-existing-binding-consistent-succeeds
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
        (bindings (list (cons (lindalog:make-variable 'x)
                              (lindalog:make-constant 'a)))))
    (is (lindalog::bindings-equal-p
         (list (cons (lindalog:make-variable 'x)
                     (lindalog:make-constant 'a)))
         (lindalog::match-atom a b bindings)))))

(test match-atom-existing-binding-inconsistent-fails
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'b))))
        (bindings (list (cons (lindalog:make-variable 'x)
                              (lindalog:make-constant 'a)))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-atom a b bindings)))))

(test match-atom-preserves-existing-bindings
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x)
                                        (lindalog:make-variable 'y))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b))))
        (bindings (list (cons (lindalog:make-variable 'x)
                              (lindalog:make-constant 'a)))))
    (is (lindalog::bindings-equal-p
         (list (cons (lindalog:make-variable 'x)
                     (lindalog:make-constant 'a))
               (cons (lindalog:make-variable 'y)
                     (lindalog:make-constant 'b)))
         (lindalog::match-atom a b bindings)))))

(test match-atom-repeated-variable-after-intervening-variable-succeeds
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x)
                                        (lindalog:make-variable 'y)
                                        (lindalog:make-variable 'x))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b)
                                        (lindalog:make-constant 'a)))))
    (is (lindalog::bindings-equal-p
         (list (cons (lindalog:make-variable 'x)
                     (lindalog:make-constant 'a))
               (cons (lindalog:make-variable 'y)
                     (lindalog:make-constant 'b)))
         (lindalog::match-atom a b lindalog::+no-bindings+)))))

(test match-atom-repeated-variable-after-intervening-variable-fails
  (let ((a (lindalog:make-atom 'p (list (lindalog:make-variable 'x)
                                        (lindalog:make-variable 'y)
                                        (lindalog:make-variable 'x))))
        (b (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                        (lindalog:make-constant 'b)
                                        (lindalog:make-constant 'c)))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-atom a b lindalog::+no-bindings+)))))
