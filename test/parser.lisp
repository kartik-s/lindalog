;;;; parser.lisp

(in-package #:lindalog/tests)

(in-suite lindalog)

(defun as-cst (form)
  (cst:cst-from-expression form))

;;; PARSE-TERM

(test parse-term-variable
  (is (lindalog:ast-equal-p
       (lindalog:make-variable '?x)
       (lindalog:parse-term (as-cst '?x)))))

(test parse-term-constant-symbol
  (is (lindalog:ast-equal-p
       (lindalog:make-constant 'a)
       (lindalog:parse-term (as-cst 'a)))))

(test parse-term-integer
  (is (lindalog:ast-equal-p
       (lindalog:make-constant 3)
       (lindalog:parse-term (as-cst 3)))))

(test parse-term-negative-integer
  (is (lindalog:ast-equal-p
       (lindalog:make-constant -3)
       (lindalog:parse-term (as-cst -3)))))

(test parse-term-large-integer
  (is (lindalog:ast-equal-p
       (lindalog:make-constant (1+ most-positive-fixnum))
       (lindalog:parse-term (as-cst (1+ most-positive-fixnum))))))

(test parse-term-t
  (is (lindalog:ast-equal-p
       (lindalog:make-constant 't)
       (lindalog:parse-term (as-cst 't)))))

(test parse-term-rejects-bare-question-mark
  (signals lindalog:syntax-error
    (lindalog:parse-term (as-cst '?))))

(test parse-term-rejects-reserved-name
  (signals lindalog:syntax-error
    (lindalog:parse-term (as-cst 'rd))))

(test parse-term-rejects-nil
  (signals lindalog:syntax-error
    (lindalog:parse-term (as-cst nil))))

(test parse-term-rejects-string
  (signals lindalog:syntax-error
    (lindalog:parse-term (as-cst "a"))))

(test parse-term-rejects-float
  (signals lindalog:syntax-error
    (lindalog:parse-term (as-cst 1.5))))

(test parse-term-rejects-ratio
  (signals lindalog:syntax-error
    (lindalog:parse-term (as-cst 1/2))))

(test parse-term-rejects-character
  (signals lindalog:syntax-error
    (lindalog:parse-term (as-cst #\a))))

(test parse-term-rejects-list
  (signals lindalog:syntax-error
    (lindalog:parse-term (as-cst '(q a)))))

(test parse-term-error-carries-term
  (let* ((input (as-cst '(p "a")))
         (term (cst:second input)))
    (handler-case
        (progn (lindalog:parse-atom input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq term (lindalog:syntax-error-cst e)))))))

;;; PARSE-ATOM

(test parse-atom-with-no-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-atom (lindalog:make-pred-ref 'p)
                           '())
       (lindalog:parse-atom (as-cst '(p))))))

(test parse-atom-with-constant-argument
  (is (lindalog:ast-equal-p
       (lindalog:make-atom (lindalog:make-pred-ref 'p)
                           (list (lindalog:make-constant 'a)))
       (lindalog:parse-atom (as-cst '(p a))))))

(test parse-atom-with-integer-argument
  (is (lindalog:ast-equal-p
       (lindalog:make-atom (lindalog:make-pred-ref 'p)
                           (list (lindalog:make-constant 2)))
       (lindalog:parse-atom (as-cst '(p 2))))))

(test parse-atom-preserves-argument-order
  (is (lindalog:ast-equal-p
       (lindalog:make-atom (lindalog:make-pred-ref 'p)
                           (list (lindalog:make-variable '?x)
                                 (lindalog:make-constant 'a)
                                 (lindalog:make-constant 2)))
       (lindalog:parse-atom (as-cst '(p ?x a 2))))))

(test parse-atom-with-repeated-variables
  (is (lindalog:ast-equal-p
       (lindalog:make-atom (lindalog:make-pred-ref 'p)
                           (list (lindalog:make-variable '?x)
                                 (lindalog:make-variable '?x)))
       (lindalog:parse-atom (as-cst '(p ?x ?x))))))

(test parse-atom-preserves-predicate
  (is (lindalog:ast-equal-p
       (lindalog:atom-predicate
        (lindalog:parse-atom (as-cst '(p 2))))
       (lindalog:make-pred-ref 'p))))

(test parse-atom-with-bare-question-mark
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(p ?)))))

(test parse-atom-rejects-non-list
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst 'p))))

(test parse-atom-rejects-empty-list
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '()))))

(test parse-atom-rejects-dotted-list
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(p . a)))))

(test parse-atom-rejects-numeric-predicate
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(3 a)))))

(test parse-atom-rejects-nil-predicate
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(nil a)))))

(test parse-atom-rejects-variable-predicate
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(?p a)))))

(test parse-atom-rejects-keyword-predicate
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(:p a)))))

(test parse-atom-rejects-reserved-predicates
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(defrule a))))
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(defpred a))))
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(deffact a))))
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(rd a))))
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(:rd a)))))

(test parse-atom-rejects-string-argument
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(p "a")))))

(test parse-atom-rejects-float-argument
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(p 1.5)))))

(test parse-atom-rejects-nested-list-argument
  (signals lindalog:syntax-error
    (lindalog:parse-atom (as-cst '(p (q a))))))

(test parse-atom-error-carries-predicate
  (let* ((input (as-cst '(3 a)))
         (pred (cst:first input)))
    (handler-case
        (progn (lindalog:parse-atom input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq pred (lindalog:syntax-error-cst e)))))))

(test parse-atom-error-message-names-culprit
  (let ((culprit '?bad))
    (handler-case
        (progn (lindalog:parse-atom (as-cst (list culprit 2))) nil)
      (lindalog:syntax-error (e)
        (is (search (prin1-to-string culprit)
                    (princ-to-string e)))))))

;;; PARSE-PREMISE

(test parse-premise-with-plain-atom
  (is (lindalog:ast-equal-p
       (lindalog:parse-premise (as-cst '(p ?x)))
       (lindalog:make-premise
        (lindalog:make-atom (lindalog:make-pred-ref 'p)
                            (list (lindalog:make-variable '?x)))
        nil))))

(test parse-premise-with-rd-wrapped-atom
  (is (lindalog:ast-equal-p
       (lindalog:parse-premise (as-cst '(:rd (p ?x))))
       (lindalog:make-premise
        (lindalog:make-atom (lindalog:make-pred-ref 'p)
                            (list (lindalog:make-variable '?x)))
        t))))

(test parse-premise-with-rd-preserves-inner-atom
  (is (lindalog:ast-equal-p
       (lindalog:premise-atom
        (lindalog:parse-premise (as-cst '(:rd (p ?x)))))
       (lindalog:make-atom (lindalog:make-pred-ref 'p)
                           (list (lindalog:make-variable '?x))))))

(test parse-premise-with-non-keyword-rd
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(rd (p))))))

(test parse-premise-rejects-non-list
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst 'p))))

(test parse-premise-rejects-number
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst 3))))

(test parse-premise-rejects-dotted-list
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(p . ?x)))))

(test parse-premise-rejects-empty-premise
 (signals lindalog:syntax-error
   (lindalog:parse-premise (as-cst nil))))

(test parse-premise-rejects-rd-without-argument
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(:rd)))))

(test parse-premise-rejects-rd-with-extra-argument
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(:rd (p) (q))))))

(test parse-premise-rejects-dotted-rd
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(:rd . p)))))

(test parse-premise-rejects-nested-rd
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(:rd (:rd p))))))

(test parse-premise-rejects-rd-with-non-list-argument
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(:rd p)))))

(test parse-premise-rejects-malformed-plain-atom
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(3 a)))))

(test parse-premise-rejects-malformed-atom-inside-rd
  (signals lindalog:syntax-error
    (lindalog:parse-premise (as-cst '(:rd (3 a))))))

(test parse-premise-error-carries-arg
  (let* ((input (as-cst '(rd (p ?x))))
         (arg (cst:first input)))
    (handler-case
        (progn (lindalog:parse-premise input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq arg (lindalog:syntax-error-cst e)))))))

(test parse-premise-atom-error-carries-predicate
  (let* ((input (as-cst '(:rd (3 a))))
         (predicate (cst:first (cst:second input))))
    (handler-case
        (progn (lindalog:parse-premise input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq predicate (lindalog:syntax-error-cst e)))))))

;;; PARSE-CONCLUSION
(test parse-conclusion-with-atom
  (is (lindalog:ast-equal-p
       (lindalog:parse-atom (as-cst '(p ?x a)))
       (lindalog:parse-conclusion (as-cst '(p ?x a))))))

(test parse-conclusion-rejects-rd-marker
  (signals lindalog:syntax-error
    (lindalog:parse-conclusion (as-cst '(:rd (p ?x))))))

(test parse-conclusion-rejects-malformed-atom
  (signals lindalog:syntax-error
    (lindalog:parse-conclusion (as-cst '(3 a)))))

(test parse-conclusion-error-carries-predicate
  (let* ((input (as-cst '(3 ?x)))
         (pred (cst:first input)))
    (handler-case
        (progn (lindalog:parse-premise input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq pred (lindalog:syntax-error-cst e)))))))

;;; PARSE-PRED-DECL

(test parse-pred-decl-with-rd-store
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo '(bar) :rd)
       (lindalog:parse-pred-decl (as-cst '(defpred foo (bar) (:store :rd)))))))

(test parse-pred-decl-with-in-store
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo '(bar) :in)
       (lindalog:parse-pred-decl (as-cst '(defpred foo (bar) (:store :in)))))))

(test parse-pred-decl-with-sig-store
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo '(bar) :sig)
       (lindalog:parse-pred-decl (as-cst '(defpred foo (bar) (:store :sig)))))))

(test parse-pred-decl-with-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo '(bar baz) :rd)
       (lindalog:parse-pred-decl (as-cst '(defpred foo (bar baz) (:store :rd)))))))

(test parse-pred-decl-with-no-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo '() :rd)
       (lindalog:parse-pred-decl (as-cst '(defpred foo () (:store :rd)))))))

(test parse-pred-decl-preserves-argument-order
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'foo '(a b c) :rd)
       (lindalog:parse-pred-decl (as-cst '(defpred foo (a b c) (:store :rd)))))))

(test parse-pred-decl-with-docstring
  (let ((a (lindalog:make-pred-decl
            'p '(a b) :rd
            :docstring "foo"))
        (b (lindalog:parse-pred-decl
            (as-cst '(defpred p (a b) (:store :rd) (:doc "foo"))))))
    (is (lindalog:ast-equal-p a b))
    (is (string= (lindalog:pred-decl-docstring a)
                 (lindalog:pred-decl-docstring b)))))

(test parse-pred-decl-rejects-duplicate-argument-names
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (a a) (:store :rd))))))

(test parse-pred-decl-rejects-dotted-form
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo . :rd)))))

(test parse-pred-decl-rejects-missing-args
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (:store :rd))))))

(test parse-pred-decl-rejects-missing-store
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (a b))))))

(test parse-pred-decl-rejects-extra-parts
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo () bar (:store :rd))))))

(test parse-pred-decl-rejects-nil-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred nil () (:store :rd))))))

(test parse-pred-decl-rejects-numeric-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred 3 () (:store :rd))))))

(test parse-pred-decl-rejects-keyword-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred :foo () (:store :rd))))))

(test parse-pred-decl-rejects-variable-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred ?foo () (:store :rd))))))

(test parse-pred-decl-rejects-reserved-name
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred rd () (:store :rd))))))

(test parse-pred-decl-rejects-nil-arg
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (nil) (:store :rd))))))

(test parse-pred-decl-rejects-nil-store
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo () (:store nil))))))

(test parse-pred-decl-rejects-nil-unknown-store
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo () (:store :bar))))))

(test parse-pred-decl-rejects-nil-non-keyword-store
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo () (:store rd))))))

(test parse-pred-decl-rejects-non-list-arguments
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo bar (:store :rd))))))

(test parse-pred-decl-rejects-dotted-arguments
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (bar . baz) (:store :rd))))))

(test parse-pred-decl-rejects-non-symbol-argument
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (bar 2) (:store :rd))))))

(test parse-pred-decl-rejects-variable-argument
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (?bar) (:store :rd))))))

(test parse-pred-decl-rejects-keyword-argument
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (:bar) (:store :rd))))))

(test parse-pred-decl-rejects-reserved-argument
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred foo (bar rd) (:store :rd))))))

(test parse-pred-decl-rejects-extra-store
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred p (a b) (:store :rd :in))))))

(test parse-pred-decl-rejects-extra-docstring
  (signals lindalog:syntax-error
    (lindalog:parse-pred-decl (as-cst '(defpred p (a b) (:store :rd) (:doc "foo" 3))))))

(test parse-pred-decl-missing-args-carries-form
  (let ((input (as-cst '(defpred foo (:store :rd)))))
    (handler-case
        (progn (lindalog:parse-pred-decl input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq input (lindalog:syntax-error-cst e)))))))

(test parse-pred-decl-missing-store-carries-form
  (let ((input (as-cst '(defpred foo (a b)))))
    (handler-case
        (progn (lindalog:parse-pred-decl input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq input (lindalog:syntax-error-cst e)))))))

(test parse-pred-decl-argument-error-carries-argument
  (let* ((input (as-cst '(defpred foo (bar 2) (:store :rd))))
         (arg (cst:second (cst:third input))))
    (handler-case
        (progn (lindalog:parse-pred-decl input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq arg (lindalog:syntax-error-cst e)))))))

(test parse-pred-decl-bad-store-error-message-names-culprit
  (let ((form '(defpred foo (x y) (:store :bar))))
    (handler-case
        (progn (lindalog:parse-pred-decl (as-cst form)) nil)
      (lindalog:syntax-error (e)
        (is (search ":BAR" (princ-to-string e)))))))

;;; PARSE-INIT-FACT

(test parse-init-fact-with-constant-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-init-fact
        (lindalog:make-atom (lindalog:make-pred-ref 'p)
                            (list (lindalog:make-constant 'a)
                                  (lindalog:make-constant 'b))))
       (lindalog:parse-init-fact (as-cst '(deffact (p a b)))))))

(test parse-init-fact-with-no-arguments
  (is (lindalog:ast-equal-p
       (lindalog:make-init-fact
        (lindalog:make-atom (lindalog:make-pred-ref 'p)
                            '()))
       (lindalog:parse-init-fact (as-cst '(deffact (p)))))))

(test parse-init-fact-with-integer-argument
  (is (lindalog:ast-equal-p
       (lindalog:make-init-fact
        (lindalog:make-atom (lindalog:make-pred-ref 'p)
                            (list (lindalog:make-constant 'a)
                                  (lindalog:make-constant 3))))
       (lindalog:parse-init-fact (as-cst '(deffact (p a 3)))))))

(test parse-init-fact-with-docstring
  (let ((a (lindalog:parse-init-fact (as-cst '(deffact (p a 3) (:doc "foo")))))
        (b (lindalog:make-init-fact
            (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                (list (lindalog:make-constant 'a)
                                      (lindalog:make-constant 3)))
            :docstring "foo")))
    (is (lindalog:ast-equal-p a b))
    (is (string= (lindalog:init-fact-docstring a)
                 (lindalog:init-fact-docstring b)))))

(test parse-init-fact-matches-parse-atom
  (let ((atom '(p a b)))
    (is (lindalog:ast-equal-p
         (lindalog:parse-atom (as-cst atom))
         (lindalog:init-fact-atom
          (lindalog:parse-init-fact (as-cst `(deffact ,atom))))))))

(test parse-init-fact-rejects-dotted-form
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact (as-cst '(deffact . p)))))

(test parse-init-fact-rejects-missing-atom
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact (as-cst '(deffact)))))

(test parse-init-fact-rejects-extra-parts
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact (as-cst '(deffact (p a b) (q c))))))

(test parse-init-fact-rejects-non-list-atom
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact (as-cst '(deffact p)))))

(test parse-init-fact-rejects-malformed-atom
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact (as-cst '(deffact (3 a))))))

(test parse-init-fact-rejects-variable
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact (as-cst '(deffact (p ?x b))))))

(test parse-init-fact-rejects-rd-marker
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact (as-cst '(deffact (:rd (p a)))))))

(test parse-init-fact-rejects-extra-docstring
  (signals lindalog:syntax-error
    (lindalog:parse-init-fact (as-cst '(deffact (p a) (:doc "foo" 3))))))

(test parse-init-fact-shape-error-carries-form
  (let ((input (as-cst '(deffact . p))))
    (handler-case
        (progn (lindalog:parse-init-fact input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq input (lindalog:syntax-error-cst e)))))))

(test parse-init-fact-groundness-error-carries-arg
  (let* ((input (as-cst '(deffact (p ?x b))))
         (arg (cst:second (cst:second input))))
    (handler-case
        (progn (lindalog:parse-init-fact input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq arg (lindalog:syntax-error-cst e)))))))

;;; PARSE-RULE

(test parse-rule-with-one-premise-and-conclusion
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                   (list (lindalog:make-constant 'a)))
               nil))
        (list (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                  (list (lindalog:make-constant 'a)))))
       (lindalog:parse-rule (as-cst '(defrule foo
                                     (:when (p a))
                                     (:then (q a))))))))

(test parse-rule-with-several-premises
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                   (list (lindalog:make-constant 'a)))
               nil)
              (lindalog:make-premise
               (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                   (list (lindalog:make-constant 'b)))
               nil)
              (lindalog:make-premise
               (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                   (list (lindalog:make-constant 'c)))
               t))
        (list (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                  (list (lindalog:make-constant 'a)))))
       (lindalog:parse-rule (as-cst '(defrule foo
                                     (:when (p a) (p b) (:rd (q c)))
                                     (:then (q a))))))))

(test parse-rule-with-rd-premise
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                   (list (lindalog:make-constant 'a)))
               nil)
              (lindalog:make-premise
               (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                   (list (lindalog:make-constant 'b)))
               t))
        (list (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                  (list (lindalog:make-constant 'a)))))
       (lindalog:parse-rule (as-cst '(defrule foo
                                     (:when (p a) (:rd (q b)))
                                     (:then (q a))))))))

(test parse-rule-with-several-conclusions
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                   (list (lindalog:make-constant 'a)))
               nil))
        (list (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                  (list (lindalog:make-constant 'a)))
              (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                  (list (lindalog:make-constant 'b)))
              (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                  (list (lindalog:make-constant 'c)))))
       (lindalog:parse-rule (as-cst '(defrule foo
                                     (:when (q a))
                                     (:then (p a) (p b) (q c))))))))

(test parse-rule-with-empty-then
  (is (lindalog:ast-equal-p
       (lindalog:make-rule
        'foo
        (list (lindalog:make-premise
               (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                   (list (lindalog:make-constant 'a)))
               nil))
        nil)
       (lindalog:parse-rule (as-cst '(defrule foo
                                     (:when (p a))
                                     (:then)))))))

(test parse-rule-clauses-in-any-order
  (is (lindalog:ast-equal-p
       (lindalog:parse-rule (as-cst '(defrule foo
                                      (:when (p a))
                                      (:then (q a)))))
       (lindalog:parse-rule (as-cst '(defrule foo
                                      (:then (q a))
                                      (:when (p a))))))))

(test parse-rule-with-docstring
  (let ((a (lindalog:make-rule
            'foo
            (list (lindalog:make-premise
                   (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                       (list (lindalog:make-constant 'a)))
                   nil))
            (list (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                      (list (lindalog:make-constant 'a))))
            :docstring "Consume (p a) and produce (q a)."))
        (b (lindalog:parse-rule (as-cst '(defrule foo
                                          (:doc "Consume (p a) and produce (q a).")
                                          (:when (p a))
                                          (:then (q a)))))))
    (is (lindalog:ast-equal-p a b))
    (is (string= (lindalog:rule-docstring a)
                 (lindalog:rule-docstring b)))))

(test parse-rule-without-docstring
  (let ((a (lindalog:make-rule
            'foo
            (list (lindalog:make-premise
                   (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                       (list (lindalog:make-constant 'a)))
                   nil))
            (list (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                      (list (lindalog:make-constant 'a))))))
        (b (lindalog:parse-rule (as-cst '(defrule foo
                                         (:when (p a))
                                         (:then (q a)))))))
    (is (lindalog:ast-equal-p a b))))

(test parse-rule-rejects-duplicate-clause
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                   (:when (p a))
                                   (:when (p a))
                                   (:then (q a)))))))

(test parse-rule-rejects-dotted-form
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo . "test")))))

(test parse-rule-rejects-nil-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule nil
                                  (:when (p a))
                                  (:then (q a)))))))

(test parse-rule-rejects-numeric-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule 34
                                  (:when (p a))
                                  (:then (q a)))))))

(test parse-rule-rejects-keyword-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule :foo
                                  (:when (p a))
                                  (:then (q a)))))))

(test parse-rule-rejects-variable-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule ?foo
                                  (:when (p a))
                                  (:then (q a)))))))

(test parse-rule-rejects-reserved-name
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule rd
                                  (:when (p a))
                                  (:then (q a)))))))

(test parse-rule-rejects-missing-when
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                  (:then (q a)))))))

(test parse-rule-rejects-missing-then
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                  (:when (p a)))))))

(test parse-rule-rejects-mispelled-clause
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                  (:when (p a))
                                  (:the (q a)))))))


(test parse-rule-rejects-non-keyword-clause
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                  (when (p a))
                                  (:then (q a)))))))

(test parse-rule-rejects-empty-when
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                  (:when)
                                  (:then (q a)))))))

(test parse-rule-rejects-dotted-clause
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                  (:when . (p a))
                                  (:then (q a)))))))

(test parse-rule-rejects-malformed-premise
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                  (:when ((p a) :rd))
                                  (:then (q a)))))))

(test parse-rule-rejects-rd-conclusion
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                  (:when (p a))
                                  (:then (:rd (q a))))))))

(test parse-rule-rejects-extra-docstring
  (signals lindalog:syntax-error
    (lindalog:parse-rule (as-cst '(defrule foo
                                   (:when (p a))
                                   (:then (q a))
                                   (:doc "foo" 3))))))

(test parse-rule-error-carries-name
  (let* ((input (as-cst '(defrule ?foo (:when (p a)) (:then q a))))
         (name (cst:second input)))
    (handler-case
        (progn (lindalog:parse-rule input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq name (lindalog:syntax-error-cst e)))))))

(test parse-rule-lhs-error-carries-keyword
  (let* ((input (as-cst '(defrule foo (:wen (p a)) (:then (q a)))))
         (keyword (cst:first (cst:third input))))
    (handler-case
        (progn (lindalog:parse-rule input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq keyword (lindalog:syntax-error-cst e)))))))

(test parse-rule-rhs-error-carries-keyword
  (let* ((input (as-cst '(defrule foo (:when (p a)) (:the (q a)))))
         (keyword (cst:first (cst:fourth input))))
    (handler-case
        (progn (lindalog:parse-rule input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq keyword (lindalog:syntax-error-cst e)))))))

(test parse-rule-clause-error-carries-predicate
  (let* ((input (as-cst '(defrule foo (:when (?x a)) (:then (q a)))))
         (pred (cst:first (cst:second (cst:third input)))))
    (handler-case
        (progn (lindalog:parse-rule input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq pred (lindalog:syntax-error-cst e)))))))

(test parse-rule-premise-error-carries-rd
  (let* ((input (as-cst '(defrule foo (:when (rd a)) (:then (q a)))))
         (rd (cst:first (cst:second (cst:third input)))))
    (handler-case
        (progn (lindalog:parse-rule input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq rd (lindalog:syntax-error-cst e)))))))

(test parse-rule-missing-clause-error-message-contains-singular
  (let* ((input (as-cst '(defrule foo (:when (p a))))))
    (handler-case
        (progn (lindalog:parse-rule input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (let ((msg (princ-to-string e)))
          (is (and (search "clause" msg)
                   (not (search "clauses" msg)))))))))

(test parse-rule-missing-clauses-error-message-contains-plural
  (let* ((input (as-cst '(defrule foo))))
    (handler-case
        (progn (lindalog:parse-rule input)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (search "clauses" (princ-to-string e)))))))

;;; PARSE-TOPLEVEL

(test parse-toplevel-parses-defpred
  (is (lindalog:ast-equal-p
       (lindalog:make-pred-decl 'p '(a b) :in)
       (lindalog:parse-toplevel (as-cst '(defpred p (a b) (:store :in)))))))

(test parse-toplevel-parses-deffact
  (is (lindalog:ast-equal-p
       (lindalog:make-init-fact
        (lindalog:make-atom (lindalog:make-pred-ref 'p)
                            (list (lindalog:make-constant 'a))))
       (lindalog:parse-toplevel (as-cst '(deffact (p a)))))))

(test parse-toplevel-parses-defrule
  (is (lindalog:ast-equal-p
       (lindalog:make-rule 'foo
                           (list (lindalog:make-premise
                                  (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                                      (list (lindalog:make-constant 'a)))
                                  nil))
                           (list (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                                     (list (lindalog:make-constant 'a)))))
       (lindalog:parse-toplevel (as-cst '(defrule foo (:when (p a)) (:then (q a))))))))

(test parse-toplevel-dispatches-by-symbol-name
  (is (lindalog:ast-equal-p
       (lindalog:make-rule 'foo
                           (list (lindalog:make-premise
                                  (lindalog:make-atom (lindalog:make-pred-ref 'p)
                                                      (list (lindalog:make-constant 'a)))
                                  nil))
                           (list (lindalog:make-atom (lindalog:make-pred-ref 'q)
                                                     (list (lindalog:make-constant 'a)))))
       (lindalog:parse-toplevel (as-cst '(#:defrule foo (:when (p a)) (:then (q a))))))))

(test parse-toplevel-rejects-atom-toplevel
  (let ((culprit (as-cst "defrule")))
    (handler-case
        (progn (lindalog:parse-toplevel culprit)
               (fail "no error signaled"))
      (lindalog:syntax-error (e)
        (is (eq culprit (lindalog:syntax-error-cst e)))
        (is (search (prin1-to-string (cst:raw culprit))
                    (princ-to-string e)))))))

(test parse-toplevel-rejects-empty-toplevel
  (signals lindalog:syntax-error
    (lindalog:parse-toplevel (as-cst '()))))

(test parse-toplevel-rejects-non-symbol-head
  (signals lindalog:syntax-error
    (lindalog:parse-toplevel (as-cst '(3 foo))))
  (signals lindalog:syntax-error
    (lindalog:parse-toplevel (as-cst '("defrule" foo (:when (p a)) (:then (q a))))))
  (signals lindalog:syntax-error
    (lindalog:parse-toplevel (as-cst '((defrule) foo (:when (p a)) (:then (q a)))))))

(test parse-toplevel-rejects-unknown-head
  (signals lindalog:syntax-error
    (lindalog:parse-toplevel (as-cst '(defrules foo (:when (p a)) (:then (q a))))))
  (signals lindalog:syntax-error
    (lindalog:parse-toplevel (as-cst '(deffacts foo (:when (p a)) (:then (q a)))))))

(test parse-toplevel-rejects-dotted-toplevel
  (signals lindalog:syntax-error
    (lindalog:parse-toplevel (as-cst '(deffact . p)))))

(test parse-toplevel-rejects-keyword-head
  (signals lindalog:syntax-error
    (lindalog:parse-toplevel (as-cst '(:deffact (p a))))))

;;; PARSE-PROGRAM

(test parse-program-no-forms
  (is (lindalog:ast-equal-p
       (lindalog:make-program '() '() '())
       (lindalog:parse-program '()))))

(test parse-program-one-form-each
  (is (lindalog:ast-equal-p
       (lindalog:make-program
        (list (lindalog:parse-pred-decl (as-cst '(defpred p (a b) (:store :in)))))
        (list (lindalog:parse-init-fact (as-cst '(deffact (p 2 a)))))
        (list (lindalog:parse-rule (as-cst '(defrule foo (:when (p ?x ?y)) (:then (p ?y ?x)))))))
       (with-input-from-string (stream "
(defpred p (a b)
  (:store :in))

(deffact (p 2 a))

(defrule foo
  (:when (p ?x ?y))
  (:then (p ?y ?x)))
")
         (lindalog:parse-program
          (lindalog:read-program stream))))))

(test parse-program-interleaved-forms
  (is (lindalog:ast-equal-p
       (lindalog:make-program
        (list (lindalog:parse-pred-decl (as-cst '(defpred p (a b) (:store :in))))
              (lindalog:parse-pred-decl (as-cst '(defpred q (x) (:store :rd)))))
        (list (lindalog:parse-init-fact (as-cst '(deffact (p 2 a)))))
        (list (lindalog:parse-rule (as-cst '(defrule foo (:when (p ?x ?y)) (:then (p ?y ?x)))))
              (lindalog:parse-rule (as-cst '(defrule bar (:when (q ?x)) (:then (p ?x ?x)))))))
       (with-input-from-string (stream "
(defpred p (a b)
  (:store :in))

(defrule foo
  (:when (p ?x ?y))
  (:then (p ?y ?x)))

(deffact (p 2 a))

(defrule bar
  (:when (q ?x))
  (:then (p ?x ?x)))

(defpred q (x)
  (:store :rd))
")
         (lindalog:parse-program
          (lindalog:read-program stream))))))

(test parse-program-rejects-one-syntax-error
  (handler-case
      (progn
        (with-input-from-string (stream "
(defpred p (a b)
  (:store :foo))")
          (lindalog:parse-program
           (lindalog:read-program stream)))
        (fail "expected a PROGRAM-SYNTAX-ERRORS"))
    (lindalog:program-syntax-errors (e)
      (is (= 1 (length (lindalog:invalid-program-errors e))))
      (let ((syntax-error (first (lindalog:invalid-program-errors e))))
        (is (typep syntax-error 'lindalog:syntax-error))
        (is (equal '(28 . 32) (lindalog:source-error-range syntax-error)))
        (is (search ":FOO" (princ-to-string syntax-error)))))))

(test parse-program-rejects-multiple-syntax-errors
  (handler-case
      (progn
        (with-input-from-string (stream "
(defpred p (a b)
  (:store :foo))

(defpred q (x)
  (:store :in))

(deffact (p 1 2))
(deffact (p rd 3))

(defrule foo
  (:then (q ?x)))

(defrule bar
  (:when (q ?x))
  (:then (p ?x ?x)))")
          (lindalog:parse-program
           (lindalog:read-program stream)))
        (fail "expected a PROGRAM-SYNTAX-ERRORS"))
    (lindalog:program-syntax-errors (e)
      (let ((errors (lindalog:invalid-program-errors e)))
        (is (= 3 (length errors)))
        (is (every (lambda (e)
                     (typep e 'lindalog:syntax-error))
                   errors))
        (is (equal '(28 . 32) (lindalog:source-error-range (first errors))))
        (is (equal '(98 . 100) (lindalog:source-error-range (second errors))))
        (is (equal '(106 . 136) (lindalog:source-error-range (third errors))))
        (is (search ":FOO" (princ-to-string (first errors))))
        (is (search "RD" (princ-to-string (second errors))))
        (is (search ":when" (princ-to-string (third errors))))
        (is (search ":FOO" (princ-to-string e)))
        (is (search "RD" (princ-to-string e)))
        (is (search ":when" (princ-to-string e)))))))

(test parse-program-rejects-bad-cst-from-expression
  (handler-case
      (progn
        (lindalog:parse-program (list (as-cst '(defpred p (rd b)
                                                (:store :in)))))
        (fail "expected a PROGRAM-SYNTAX-ERRORS"))
    (lindalog:program-syntax-errors (e)
      (let ((errors (lindalog:invalid-program-errors e)))
        (is (= 1 (length errors)))
        (is (typep (first errors) 'lindalog:syntax-error))
        (is (equal nil (lindalog:source-error-range (first errors))))
        (is (string= "predicate argument is reserved: RD" (princ-to-string (first errors))))))))

(test parse-program-rejects-reader-error
  (signals lindalog:read-error
    (with-input-from-string (stream "(defpred p (a b) (:store :in)")
      (lindalog:parse-program
       (lindalog:read-program stream)))))
