;;;; parser.lisp

(in-package #:lindalog/tests)

(in-suite lindalog)

;;; PARSE-ATOM

(test parse-atom-with-no-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-atom 'p '())
       (lindalog:parse-atom '(p)))))

(test parse-atom-with-constant-argument
  (is (lindalog:ast-equal-p
       (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
       (lindalog:parse-atom '(p a)))))

(test parse-atom-with-integer-argument
  (is (lindalog:ast-equal-p
       (lindalog:make-atom 'p (list (lindalog:make-constant 2)))
       (lindalog:parse-atom '(p 2)))))

(test parse-atom-preserves-argument-order
  (is (lindalog:ast-equal-p
       (lindalog:make-atom 'p (list (lindalog:make-variable '?x)
                                    (lindalog:make-constant 'a)
                                    (lindalog:make-constant 2)))
       (lindalog:parse-atom '(p ?x a 2)))))

(test parse-atom-with-repeated-variables
  (is (lindalog:ast-equal-p
       (lindalog:make-atom 'p (list (lindalog:make-variable '?x)
                                    (lindalog:make-variable '?x)))
       (lindalog:parse-atom '(p ?x ?x)))))

(test parse-atom-preserves-predicate
  (is (eq (lindalog:atom-predicate
           (lindalog:parse-atom '(p 2)))
          'p)))

(test parse-atom-with-bare-question-mark
  (signals lindalog:syntax-error (lindalog:parse-atom '(p ?))))

(test parse-atom-rejects-non-list
  (signals lindalog:syntax-error (lindalog:parse-atom 'p)))

(test parse-atom-rejects-empty-list
  (signals lindalog:syntax-error (lindalog:parse-atom '())))

(test parse-atom-rejects-dotted-list
  (signals lindalog:syntax-error (lindalog:parse-atom '(p . a))))

(test parse-atom-rejects-numeric-predicate
  (signals lindalog:syntax-error (lindalog:parse-atom '(3 a))))

(test parse-atom-rejects-nil-predicate
  (signals lindalog:syntax-error (lindalog:parse-atom '(nil a))))

(test parse-atom-rejects-variable-predicate
  (signals lindalog:syntax-error (lindalog:parse-atom '(?p a))))

(test parse-atom-rejects-keyword-predicate
  (signals lindalog:syntax-error (lindalog:parse-atom '(:p a))))

(test parse-atom-rejects-reserved-predicates
  (signals lindalog:syntax-error (lindalog:parse-atom '(defrule a)))
  (signals lindalog:syntax-error (lindalog:parse-atom '(defpredicate a)))
  (signals lindalog:syntax-error (lindalog:parse-atom '(:rd a))))

(test parse-atom-rejects-string-argument
  (signals lindalog:syntax-error (lindalog:parse-atom '(p "a"))))

(test parse-atom-rejects-float-argument
  (signals lindalog:syntax-error (lindalog:parse-atom '(p 1.5))))

(test parse-atom-rejects-nested-list-argument
  (signals lindalog:syntax-error (lindalog:parse-atom '(p (q a)))))

(test parse-atom-error-carries-form
  (let ((form '(3 a)))
    (handler-case
        (lindalog:parse-atom form)
      (lindalog:syntax-error (e)
        (is (typep e 'lindalog:syntax-error))
        (is (eq form (lindalog::source-error-form e)))))))

(test parse-atom-error-message-names-culprit
  (let ((culprit '?bad))
    (handler-case
        (lindalog:parse-atom (list culprit 2))
      (lindalog:syntax-error (e)
        (is (typep e 'lindalog:syntax-error))
        (is (search (prin1-to-string culprit)
                    (princ-to-string e)))))))
