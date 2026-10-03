;;;; parser.lisp

(in-package #:lindalog/tests)

(in-suite lindalog)

;;; PARSE-TERM

(test parse-term-variable
  (is (lindalog:ast-equal-p
       (lindalog:make-variable '?x)
       (lindalog:parse-term '?x nil))))

(test parse-term-constant-symbol
  (is (lindalog:ast-equal-p
       (lindalog:make-constant 'a)
       (lindalog:parse-term 'a nil))))

(test parse-term-integer
  (is (lindalog:ast-equal-p
       (lindalog:make-constant 3)
       (lindalog:parse-term 3 nil))))

(test parse-term-negative-integer
  (is (lindalog:ast-equal-p
       (lindalog:make-constant -3)
       (lindalog:parse-term -3 nil))))

(test parse-term-large-integer
  (is (lindalog:ast-equal-p
       (lindalog:make-constant (1+ most-positive-fixnum))
       (lindalog:parse-term (1+ most-positive-fixnum) nil))))

(test parse-term-t
  (is (lindalog:ast-equal-p
       (lindalog:make-constant 't)
       (lindalog:parse-term 't nil))))

(test parse-term-rejects-bare-question-mark
  (signals lindalog:syntax-error
    (lindalog:parse-term '? nil)))

(test parse-term-rejects-nil
  (signals lindalog:syntax-error
    (lindalog:parse-term nil nil)))

(test parse-term-rejects-string
  (signals lindalog:syntax-error
    (lindalog:parse-term "a" nil)))

(test parse-term-rejects-float
  (signals lindalog:syntax-error
    (lindalog:parse-term 1.5 nil)))

(test parse-term-rejects-ratio
  (signals lindalog:syntax-error
    (lindalog:parse-term 1/2 nil)))

(test parse-term-rejects-character
  (signals lindalog:syntax-error
    (lindalog:parse-term #\a nil)))

(test parse-term-rejects-list
  (signals lindalog:syntax-error
    (lindalog:parse-term '(q a) nil)))

(test parse-term-error-carries-enclosing-atom
  (let ((form '(p "a")))
    (handler-case
        (progn (lindalog:parse-atom form) nil)
      (lindalog:syntax-error (e)
        (is (eq form (lindalog::source-error-form e)))))))

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
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(p ?))))

(test parse-atom-rejects-non-list
  (signals lindalog:syntax-error
    (lindalog:parse-atom 'p)))

(test parse-atom-rejects-empty-list
  (signals lindalog:syntax-error
    (lindalog:parse-atom '())))

(test parse-atom-rejects-dotted-list
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(p . a))))

(test parse-atom-rejects-numeric-predicate
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(3 a))))

(test parse-atom-rejects-nil-predicate
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(nil a))))

(test parse-atom-rejects-variable-predicate
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(?p a))))

(test parse-atom-rejects-keyword-predicate
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(:p a))))

(test parse-atom-rejects-reserved-predicates
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(defrule a)))
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(defpred a)))
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(deffact a)))
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(rd a)))
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(:rd a))))

(test parse-atom-rejects-string-argument
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(p "a"))))

(test parse-atom-rejects-float-argument
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(p 1.5))))

(test parse-atom-rejects-nested-list-argument
  (signals lindalog:syntax-error
    (lindalog:parse-atom '(p (q a)))))

(test parse-atom-error-carries-form
  (let ((form '(3 a)))
    (handler-case
        (progn (lindalog:parse-atom form) nil)
      (lindalog:syntax-error (e)
        (is (eq form (lindalog::source-error-form e)))))))

(test parse-atom-error-message-names-culprit
  (let ((culprit '?bad))
    (handler-case
        (progn (lindalog:parse-atom (list culprit 2)) nil)
      (lindalog:syntax-error (e)
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
        (progn (lindalog:parse-premise form) nil)
      (lindalog:syntax-error (e)
        (is (eq form (lindalog::source-error-form e)))))))

(test parse-premise-inner-error-carries-inner-form
  (let ((culprit '(3 a)))
    (handler-case
        (progn (lindalog:parse-premise `(:rd ,culprit)) nil)
      (lindalog:syntax-error (e)
        (is (search "3" (princ-to-string e)))))))

;;; PARSE-CONCLUSION
(test parse-conclusion-with-atom
  (is (lindalog:ast-equal-p
       (lindalog:parse-atom '(p ?x a))
       (lindalog:parse-conclusion '(p ?x a)))))

(test parse-conclusion-rejects-rd-marker
  (signals lindalog:syntax-error
    (lindalog:parse-conclusion '(:rd (p ?x)))))

(test parse-conclusion-rejects-malformed-atom
  (signals lindalog:syntax-error
    (lindalog:parse-conclusion '(3 a))))

(test parse-conclusion-error-carries-form
  (let ((form '(3 ?x)))
    (handler-case
        (progn (lindalog:parse-premise form) nil)
      (lindalog:syntax-error (e)
        (is (eq form (lindalog::source-error-form e)))))))

;;; PARSE-PRED-DECL

(test parse-pred-decl-with-rd-store
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo :rd '(bar))
       (lindalog:parse-pred-decl '(defpred foo :rd (bar))))))

(test parse-pred-decl-with-in-store
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo :in '(bar))
       (lindalog:parse-pred-decl '(defpred foo :in (bar))))))

(test parse-pred-decl-with-sub-store
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo :sub '(bar))
       (lindalog:parse-pred-decl '(defpred foo :sub (bar))))))

(test parse-pred-decl-with-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo :rd '(bar baz))
       (lindalog:parse-pred-decl '(defpred foo :rd (bar baz))))))

(test parse-pred-decl-with-no-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo :rd '())
       (lindalog:parse-pred-decl '(defpred foo :rd ())))))

(test parse-pred-decl-preserves-argument-order
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo :rd '(a b c))
       (lindalog:parse-pred-decl '(defpred foo :rd (a b c))))))

(test parse-pred-decl-rejects-duplicate-argument-names
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd (a a)))))

(test parse-pred-decl-rejects-dotted-form
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo . :rd))))

(test parse-pred-decl-rejects-missing-parts
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd))))

(test parse-pred-decl-rejects-extra-parts
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd () bar))))

(test parse-pred-decl-rejects-nil-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred nil :rd ()))))

(test parse-pred-decl-rejects-numeric-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred 3 :rd ()))))

(test parse-pred-decl-rejects-keyword-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred :foo :rd ()))))

(test parse-pred-decl-rejects-variable-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred ?foo :rd ()))))

(test parse-pred-decl-rejects-reserved-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred rd :rd ()))))

(test parse-pred-decl-rejects-nil-store
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo nil ()))))

(test parse-pred-decl-rejects-nil-unknown-store
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :bar ()))))

(test parse-pred-decl-rejects-nil-non-keyword-store
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo rd ()))))

(test parse-pred-decl-rejects-non-list-arguments
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd bar))))

(test parse-pred-decl-rejects-dotted-arguments
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd (bar . baz)))))

(test parse-pred-decl-rejects-non-symbol-argument
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd (bar 2)))))

(test parse-pred-decl-rejects-variable-argument
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd (?bar)))))

(test parse-pred-decl-rejects-keyword-argument
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd (:bar)))))

(test parse-pred-decl-rejects-reserved-argument
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl '(defpred foo :rd (bar rd)))))

(test parse-pred-decl-shape-error-carries-form
  (let ((form '(defpred foo :rd)))
    (handler-case
        (progn (lindalog:parse-pred-decl form) nil)
      (lindalog:syntax-error (e)
        (is (eq form (lindalog::source-error-form e)))))))

(test parse-pred-decl-argument-error-carries-argument-list
  (let ((form '(defpred foo :rd (bar 2))))
    (handler-case
        (progn (lindalog:parse-pred-decl form) nil)
      (lindalog:syntax-error (e)
        (is (eq form (lindalog::source-error-form e)))))))

(test parse-pred-decl-error-message-names-culprit
  (let ((form '(defpred foo :bar (x y))))
    (handler-case
        (progn (lindalog:parse-pred-decl form) nil)
      (lindalog:syntax-error (e)
        (is (search ":BAR" (princ-to-string e)))))))

;;; PARSE-INIT-FACT

(test parse-init-fact-with-constant-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-init-fact
        (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                     (lindalog:make-constant 'b))))
       (lindalog:parse-init-fact '(deffact (p a b))))))

(test parse-init-fact-with-no-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-init-fact
        (lindalog:make-atom 'p '()))
       (lindalog:parse-init-fact '(deffact (p))))))

(test parse-init-fact-with-integer-argument
  (is (lindalog:ast-equal-p
       (lindalog:make-init-fact
        (lindalog:make-atom 'p (list (lindalog:make-constant 'a)
                                     (lindalog:make-constant 3))))
       (lindalog:parse-init-fact '(deffact (p a 3))))))

(test parse-init-fact-matches-parse-atom
  (let ((atom '(p a b)))
    (is (lindalog:ast-equal-p
         (lindalog:parse-atom atom)
         (lindalog:init-fact-atom
          (lindalog:parse-init-fact `(deffact ,atom)))))))

(test parse-init-fact-rejects-dotted-form
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact '(deffact . p))))

(test parse-init-fact-rejects-missing-atom
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact '(deffact))))

(test parse-init-fact-rejects-extra-parts
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact '(deffact (p a b) (q c)))))

(test parse-init-fact-rejects-non-list-atom
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact '(deffact p))))

(test parse-init-fact-rejects-malformed-atom
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact '(deffact (3 a)))))

(test parse-init-fact-rejects-variable
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact '(deffact (p ?x b)))))

(test parse-init-fact-rejects-rd-marker
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact '(deffact (:rd (p a))))))

(test parse-init-fact-shape-error-carries-form
  (let ((form '(deffact . p)))
    (handler-case
        (progn (lindalog:parse-init-fact form) nil)
      (lindalog:syntax-error (e)
        (is (eq form (lindalog::source-error-form e)))))))

(test parse-init-fact-groundness-error-carries-atom
  (let ((atom '(p ?x b)))
    (handler-case
        (progn (lindalog:parse-init-fact `(deffact ,atom)) nil)
      (lindalog:syntax-error (e)
        (is (eq atom (lindalog::source-error-form e)))))))

(test parse-init-fact-groundness-message-names-variable
  (let ((culprit '?x))
    (handler-case
        (progn (lindalog:parse-init-fact `(deffact (p ,culprit b))) nil)
      (lindalog:syntax-error (e)
        (is (search (prin1-to-string culprit)
                    (princ-to-string e)))))))

;;; PARSE-RULE

(test parse-rule-with-one-premise-and-conclusion
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
               nil))
        (list (lindalog:make-atom 'q (list (lindalog:make-constant 'a)))))
       (lindalog:parse-rule '(defrule foo
                              (:when (p a))
                              (:then (q a)))))))

(test parse-rule-with-several-premises
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom'p (list (lindalog:make-constant 'a)))
               nil)
              (lindalog:make-premise
               (lindalog:make-atom 'p (list (lindalog:make-constant 'b)))
               nil)
              (lindalog:make-premise
               (lindalog:make-atom 'q (list (lindalog:make-constant 'c)))
               t))
        (list (lindalog:make-atom 'q (list (lindalog:make-constant 'a)))))
       (lindalog:parse-rule '(defrule foo
                              (:when (p a) (p b) (:rd (q c)))
                              (:then (q a)))))))

(test parse-rule-with-rd-premise
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
               nil)
              (lindalog:make-premise
               (lindalog:make-atom 'q (list (lindalog:make-constant 'b)))
               t))
        (list (lindalog:make-atom 'q (list (lindalog:make-constant 'a)))))
       (lindalog:parse-rule '(defrule foo
                              (:when (p a) (:rd (q b)))
                              (:then (q a)))))))

(test parse-rule-with-several-conclusions
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom 'q (list (lindalog:make-constant 'a)))
               nil))
        (list (lindalog:make-atom'p (list (lindalog:make-constant 'a)))
              (lindalog:make-atom 'p (list (lindalog:make-constant 'b)))
              (lindalog:make-atom 'q (list (lindalog:make-constant 'c)))))
       (lindalog:parse-rule '(defrule foo
                              (:when (q a))
                              (:then (p a) (p b) (q c)))))))

(test parse-rule-with-empty-then
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
               nil))
        nil)
       (lindalog:parse-rule '(defrule foo
                              (:when (p a))
                              (:then))))))

(test parse-rule-with-docstring
  (let ((a (lindalog:make-rule
            'foo
            (list (lindalog:make-premise
                   (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
                   nil))
            (list (lindalog:make-atom 'q (list (lindalog:make-constant 'a))))
            :docstring "Consume (p a) and produce (q a)."))
        (b (lindalog:parse-rule '(defrule foo
                                  "Consume (p a) and produce (q a)."
                                  (:when (p a))
                                  (:then (q a))))))
    (is (lindalog:ast-equal-p a b))
    (is (string= (lindalog:rule-docstring a)
                 (lindalog:rule-docstring b)))))

(test parse-rule-without-docstring
  (let ((a (lindalog:make-rule
            'foo
            (list (lindalog:make-premise
                   (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
                   nil))
            (list (lindalog:make-atom 'q (list (lindalog:make-constant 'a))))
            :docstring "Consume (p a) and produce (q a)."))
        (b (lindalog:parse-rule '(defrule foo
                                  "Consume (p a) and produce (q a)."
                                  (:when (p a))
                                  (:then (q a))))))
    (is (lindalog:ast-equal-p a b))
    (is (eq (lindalog:rule-docstring a)
            (lindalog:rule-docstring b)))))

(test parse-rule-rejects-dotted-form
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule foo . "test"))))

(test parse-rule-rejects-nil-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule nil
                           (:when (p a))
                           (:then (q a))))))

(test parse-rule-rejects-numeric-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule 34
                           (:when (p a))
                           (:then (q a))))))

(test parse-rule-rejects-keyword-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule :foo
                           (:when (p a))
                           (:then (q a))))))

(test parse-rule-rejects-variable-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule ?foo
                           (:when (p a))
                           (:then (q a))))))

(test parse-rule-rejects-reserved-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when (p a))
                           (:then (q a))))))

(test parse-rule-rejects-missing-when
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:then (q a))))))

(test parse-rule-rejects-missing-then
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when (p a))))))

(test parse-rule-rejects-clauses-out-of-order
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:then (q a))
                           (:when (p a))))))

(test parse-rule-rejects-mispelled-clause
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when (p a))
                           (:the (q a))))))

(test parse-rule-rejects-mispelled-clause
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when (p a))
                           (:the (q a))))))

(test parse-rule-rejects-non-keyword-clause
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (when (p a))
                           (:then (q a))
                           (:when (p b))))))

(test parse-rule-rejects-misplaced-docstring
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when (p a))
                           (:then (q a))
                           "Consume (p a) and produce (q a)."))))

(test parse-rule-rejects-empty-when
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when)
                           (:then (q a))))))

(test parse-rule-rejects-dotted-clause
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when . (p a))
                           (:then (q a))))))

(test parse-rule-rejects-malformed-premise
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when ((p a) :rd))
                           (:then (q a))))))

(test parse-rule-rejects-rd-conclusion
  (signals lindalog:syntax-error
    (lindalog:parse-rule '(defrule rd
                           (:when (p a))
                           (:then (:rd (q a)))))))

(test parse-rule-error-carries-form
  (let ((form '(defrule ?foo (:when (p a)) (:then q a))))
    (handler-case
        (progn (lindalog:parse-rule form) nil)
      (lindalog:syntax-error (e)
        (is (eq form (lindalog::source-error-form e)))))))

(test parse-rule-error-carries-form
  (let* ((clause '(:wen (p a))))
    (handler-case
        (progn (lindalog:parse-rule `(defrule foo ,clause (:then q a))) nil)
      (lindalog:syntax-error (e)
        (is (eq clause (lindalog::source-error-form e)))))))

(test parse-rule-clause-error-carries-clause
  (let ((culprit '?bad))
    (handler-case
        (progn (lindalog:parse-atom (list culprit 2)) nil)
      (lindalog:syntax-error (e)
        (is (search (prin1-to-string culprit)
                    (princ-to-string e)))))))

(test parse-rule-premise-error-carries-premise
  (let ((culprit '(rd (p a))))
    (handler-case
        (progn (lindalog:parse-rule `(defrule foo (:when ,culprit) (:then (q a)))) nil)
      (lindalog:syntax-error (e)
        (is (search (prin1-to-string culprit)
                    (princ-to-string e)))))))
