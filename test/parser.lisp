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
  (signals lindalog:syntax-error (lindalog:parse-atom '(rd a)))
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

;;; PARSE-PREMISE

(test parse-premise-with-plain-atom
  (is (lindalog:ast-equal-p
       (lindalog:parse-premise '(p ?x))
       (lindalog:make-premise
        (lindalog:make-atom 'p (list (lindalog:make-variable '?x)))
        nil))))

(test parse-premise-with-rd-wrapped-atom
  (is (lindalog:ast-equal-p
       (lindalog:parse-premise '(:rd (p ?x)))
       (lindalog:make-premise
        (lindalog:make-atom 'p (list (lindalog:make-variable '?x)))
        t))))

(test parse-premise-with-rd-preserves-inner-atom
  (is (lindalog:ast-equal-p
       (lindalog:premise-atom
        (lindalog:parse-premise '(:rd (p ?x))))
       (lindalog:make-atom 'p (list (lindalog:make-variable '?x))))))

(test parse-premise-with-non-keyword-rd
  (signals lindalog:syntax-error
   (lindalog:parse-premise '(rd (p)))))

(test parse-premise-rejects-non-list
  (signals lindalog:syntax-error
   (lindalog:parse-premise 'p)))

(test parse-premise-rejects-number
  (signals lindalog:syntax-error
    (lindalog:parse-premise 3)))

(test parse-premise-rejects-dotted-list
  (signals lindalog:syntax-error
    (lindalog:parse-premise '(p . ?x))))

(test parse-premise-rejects-empty-premise
 (signals lindalog:syntax-error
   (lindalog:parse-premise nil)))

(test parse-premise-rejects-rd-without-argument
  (signals lindalog:syntax-error
    (lindalog:parse-premise '(:rd))))

(test parse-premise-rejects-rd-with-extra-argument
  (signals lindalog:syntax-error
    (lindalog:parse-premise '(:rd (p) (q)))))

(test parse-premise-rejects-dotted-rd
  (signals lindalog:syntax-error
    (lindalog:parse-premise '(:rd . p))))

(test parse-premise-rejects-nested-rd
  (signals lindalog:syntax-error
    (lindalog:parse-premise '(:rd (:rd p)))))

(test parse-premise-rejects-rd-with-non-list-argument
  (signals lindalog:syntax-error
    (lindalog:parse-premise '(:rd p))))

(test parse-premise-rejects-malformed-plain-atom
  (signals lindalog:syntax-error
    (lindalog:parse-premise '(3 a))))

(test parse-premise-rejects-malformed-atom-inside-rd
  (signals lindalog:syntax-error
    (lindalog:parse-premise '(:rd (3 a)))))

(test parse-premise-error-carries-form
  (let ((form '(rd (p ?x))))
    (handler-case
        (lindalog:parse-premise form)
      (lindalog:syntax-error (e)
        (is (typep e 'lindalog:syntax-error))
        (is (eq form (lindalog::source-error-form e)))))))

(test parse-premise-inner-error-carries-inner-form
  (let ((culprit '(3 a)))
    (handler-case
        (lindalog:parse-premise `(:rd ,culprit))
      (lindalog:syntax-error (e)
        (is (typep e 'lindalog:syntax-error))
        (is (search "3" (princ-to-string e)))))))
