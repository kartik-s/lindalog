;;;; tests.lisp

(in-package :lindalog/tests)

(in-suite lindalog)

(test constant-with-symbol-value
  (let ((c (lindalog:make-constant 'x)))
    (is (lindalog:constant-p c))
    (is (eql 'x (lindalog:constant-value c)))))

(test constant-with-number-value
  (let ((c (lindalog:make-constant 4)))
    (is (lindalog:constant-p c))
    (is (eql 4 (lindalog:constant-value c)))))

(test variable-with-symbol-name
  (let ((v (lindalog:make-variable 'x)))
    (is (lindalog:variable-p v))
    (is (eq 'x (lindalog:variable-name v)))))

(test atom-with-predicate-and-arguments
  (let ((a (lindalog:make-atom
            'at
            (list (lindalog:make-constant 'player)
                  (lindalog:make-constant 'gate)))))
    (is (lindalog:atom-p a))
    (is (eq 'at (lindalog:atom-predicate a)))
    (is (eq 'player (lindalog:constant-value (first (lindalog:atom-args a)))))
    (is (eq 'gate (lindalog:constant-value (second (lindalog:atom-args a)))))))

(test premise-with-atom-and-rd-p
  (let* ((a (lindalog:make-atom
             'at
             (list (lindalog:make-constant 'player)
                   (lindalog:make-constant 'gate))))
         (p (lindalog:make-premise a t)))
    (is (lindalog:atom-p (lindalog:premise-atom p)))
    (is (eq 'at (lindalog:atom-predicate (lindalog:premise-atom p))))
    (is (eq 'player (lindalog:constant-value (first (lindalog:atom-args (lindalog:premise-atom p))))))
    (is (eq 'gate (lindalog:constant-value (second (lindalog:atom-args (lindalog:premise-atom p))))))
    (is (lindalog:premise-rd-p p))))

