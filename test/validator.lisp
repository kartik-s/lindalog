;;;; validator.lisp

(in-package #:lindalog/tests)

(in-suite lindalog)

(test validate-program-empty-program
  (with-input-from-string (stream "")
    (let ((program (lindalog:parse-program
                    (lindalog:read-program stream))))
      (is (eq program (lindalog:validate-program program))))))

(test validate-program-valid-program
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
    (let ((program (lindalog:parse-program
             (lindalog:read-program stream))))
      (is (eq program (lindalog:validate-program program))))))

(defun validation-errors (s)
  "Read, parse, and validate the program S, returning a list of the
semantic errors found."
  (handler-case
      (progn
        (with-input-from-string (stream s)
          (lindalog:validate-program
           (lindalog:parse-program
            (lindalog:read-program stream))))
        nil)
    (lindalog:program-semantic-errors (e)
      (lindalog:invalid-program-errors e))))

(defun covered-text (node s)
  "Return the substring of S covered by the AST node NODE."
  (let ((range (cst:source (lindalog:ast-node-cst node))))
    (if range
        (subseq s (car range) (cdr range))
        "")))

(test validate-program-rejects-duplicate-predicate
  (let* ((first-decl "
(defpred p (a b)
  (:store :in))
")
         (second-decl "
(defpred p (x)
  (:store :rd))
")
         (program (concatenate 'string
                               first-decl
                               second-decl))
         (errors (validation-errors program)))
    (is (= 1 (length errors)))
    (let ((e1 (first errors)))
      (is (typep e1 'lindalog:duplicate-predicate))
      (is (string= (string-trim '(#\Newline) second-decl)
                   (covered-text (lindalog:semantic-error-ast e1) program)))
      (let ((related (lindalog:semantic-error-related e1)))
        (is (= 1 (length related)))
        (is (string= (string-trim '(#\Newline) first-decl)
                     (covered-text (car (first related)) program)))))))

(test validate-program-rejects-duplicate-rule
  (let* ((first-decl "
(defpred p (a b)
  (:store :in))
")
         (second-decl "
(defrule foo
  (:when (p ?x ?y))
  (:then (p ?y ?x)))
")
         (third-decl "
(defrule foo
  (:when (p ?x 3))
  (:then (p 2 ?x)))
")
         (program (concatenate 'string
                               first-decl
                               second-decl
                               third-decl))
         (errors (validation-errors program)))
    (is (= 1 (length errors)))
    (let ((e1 (first errors)))
      (is (typep e1 'lindalog:duplicate-rule))
      (is (string= (string-trim '(#\Newline) third-decl)
                   (covered-text (lindalog:semantic-error-ast e1) program)))
      (let ((related (lindalog:semantic-error-related e1)))
        (is (= 1 (length related)))
        (is (string= (string-trim '(#\Newline) second-decl)
                     (covered-text (car (first related)) program)))))))

(test validate-program-rejects-two-duplicate-predicates
  (let* ((first-decl "
(defpred p (a b)
  (:store :in))
")
         (second-decl "
(defpred p (x)
  (:store :rd))
")
         (third-decl "
(defpred p (x y z)
  (:store :rd))
")
         (program (concatenate 'string
                               first-decl
                               second-decl
                               third-decl))
         (errors (validation-errors program)))
    (is (= 2 (length errors)))
    (let ((e1 (first errors))
          (e2 (second errors)))
      (is (typep e1 'lindalog:duplicate-predicate))
      (is (string= (string-trim '(#\Newline) second-decl)
                   (covered-text (lindalog:semantic-error-ast e1) program)))
      (let ((related (lindalog:semantic-error-related e1)))
        (is (= 1 (length related)))
        (is (string= (string-trim '(#\Newline) first-decl)
                     (covered-text (car (first related)) program))))
      (is (typep e2 'lindalog:duplicate-predicate))
      (is (string= (string-trim '(#\Newline) third-decl)
                   (covered-text (lindalog:semantic-error-ast e2) program)))
      (let ((related (lindalog:semantic-error-related e2)))
        (is (= 1 (length related)))
        (is (string= (string-trim '(#\Newline) first-decl)
                     (covered-text (car (first related)) program)))))))

(test validate-program-rejects-duplicate-predicates-with-reference
  (let* ((first-decl "
(defpred p (a b)
  (:store :in))
")
         (second-decl "
(defpred p (x)
  (:store :rd))
")
         (fact "
(deffact (p 1 2))
")
         (program (concatenate 'string
                               first-decl
                               second-decl
                               fact))
         (errors (validation-errors program)))
    (is (= 1 (length errors)))
    (let ((e1 (first errors)))
      (is (typep e1 'lindalog:duplicate-predicate))
      (is (string= (string-trim '(#\Newline) second-decl)
                   (covered-text (lindalog:semantic-error-ast e1) program))))))

(test validate-program-rejects-undeclared-predicate-in-fact
  (let* ((first-decl "
(defpred p (a b)
  (:store :in))
")
         (fact "
(deffact (q 1 2))
")
         (program (concatenate 'string
                               first-decl
                               fact))
         (errors (validation-errors program)))
    (is (= 1 (length errors)))
    (let ((e1 (first errors)))
      (is (typep e1 'lindalog:undeclared-predicate))
      (is (string= "q" (covered-text (lindalog:semantic-error-ast e1) program))))))

(test validate-program-rejects-arity-mismatch
  (let* ((first-decl "
(defpred p (a b)
  (:store :in))
")
         (rule "
(defrule foo
  (:when (p 1)
         (p ?x ?y ?z))
  (:then (p ?z ?y)))
")
         (program (concatenate 'string
                               first-decl
                               rule))
         (errors (validation-errors program)))
    (is (= 2 (length errors)))
    (let ((e1 (first errors))
          (e2 (second errors)))
      (is (typep e1 'lindalog:arity-mismatch))
      (is (string= "(p 1)" (covered-text (lindalog:semantic-error-ast e1) program)))
      (let ((related1 (lindalog:semantic-error-related e1)))
        (is (not (null related1)))
        (is (string= (string-trim '(#\Newline) first-decl)
                     (covered-text (car (first related1)) program))))
      (is (typep e2 'lindalog:arity-mismatch))
      (is (string= "(p ?x ?y ?z)" (covered-text (lindalog:semantic-error-ast e2) program)))
      (let ((related2 (lindalog:semantic-error-related e2)))
        (is (not (null related2)))
        (is (string= (string-trim '(#\Newline) first-decl)
                     (covered-text (car (first related2)) program)))))))

(test validate-program-correct-arities
  (let* ((first-decl "
(defpred p (a b)
  (:store :in))
")
         (second-decl "
(defpred q ()
  (:store :in))
")
         (rule "
(defrule foo
  (:when (p 1 ?x)
         (q))
  (:then (p ?x ?x)))
")
         (program (concatenate 'string
                               first-decl
                               second-decl
                               rule))
         (errors (validation-errors program)))
    (is (= 0 (length errors)))))

(test validate-program-rejects-non-ground-fact
  (let* ((first-decl "
(defpred p (a b c)
  (:store :in))
")
         (fact "
(deffact (p ?x 2 ?z))
")
         (program (concatenate 'string
                               first-decl
                               fact))
         (errors (validation-errors program)))
    (is (= 2 (length errors)))
    (let ((e1 (first errors))
          (e2 (second errors)))
      (is (typep e1 'lindalog:variable-in-fact))
      (is (string= "?x" (covered-text (lindalog:semantic-error-ast e1) program)))
      (is (typep e2 'lindalog:variable-in-fact))
      (is (string= "?z" (covered-text (lindalog:semantic-error-ast e2) program))))))

(test validate-program-rejects-unbound-variable
  (let* ((first-decl "
(defpred p (a b c)
  (:store :in))
")
         (rule "
(defrule foo
  (:when (p ?x ?y ?y))
  (:then (p ?x ?y ?z)))
")
         (program (concatenate 'string
                               first-decl
                               rule))
         (errors (validation-errors program)))
    (is (= 1 (length errors)))
    (let ((e1 (first errors)))
      (is (typep e1 'lindalog:unbound-variable))
      (is (string= "?z" (covered-text (lindalog:semantic-error-ast e1) program))))))

(test validate-program-rejects-unbound-variable-multiple-times
  (let* ((first-decl "
(defpred p (a b c)
  (:store :in))
")
         (rule "
(defrule foo
  (:when (p ?x ?y ?z))
  (:then (p ?a ?a ?a)))
")
         (program (concatenate 'string
                               first-decl
                               rule))
         (errors (validation-errors program)))
    (is (= 1 (length errors)))
    (let ((e1 (first errors)))
      (is (typep e1 'lindalog:unbound-variable))
      (is (string= "?a" (covered-text (lindalog:semantic-error-ast e1) program))))))

(test validate-program-rejects-multiple-unbound-variables
  (let* ((first-decl "
(defpred p (a b c)
  (:store :in))
")
         (rule "
(defrule foo
  (:when (p ?x ?y ?z))
  (:then (p ?a ?b ?c)))
")
         (program (concatenate 'string
                               first-decl
                               rule))
         (errors (validation-errors program)))
    (is (= 3 (length errors)))
    (let ((e1 (first errors))
          (e2 (second errors))
          (e3 (third errors)))
      (is (typep e1 'lindalog:unbound-variable))
      (is (string= "?a" (covered-text (lindalog:semantic-error-ast e1) program)))
      (is (typep e2 'lindalog:unbound-variable))
      (is (string= "?b" (covered-text (lindalog:semantic-error-ast e2) program)))
      (is (typep e3 'lindalog:unbound-variable))
      (is (string= "?c" (covered-text (lindalog:semantic-error-ast e3) program))))))

(test validate-program-variable-bound-by-rd-premise
  (let* ((first-decl "
(defpred p (a b)
  (:store :in))
")
         (rule "
(defrule foo
  (:when (:rd (p ?x ?y)))
  (:then (p ?y ?x)))
")
         (program (concatenate 'string
                               first-decl
                               rule))
         (errors (validation-errors program)))
    (is (= 0 (length errors)))))

(test validate-program-rejects-redundant-rd
  (let* ((first-decl "
(defpred p (a b)
  (:store :rd))
")
         (second-decl "
(defpred q (x)
  (:store :sig))
")
         (rule "
(defrule foo
  (:when (:rd (p ?x ?y))
         (:rd (q ?x)))
  (:then (p ?y ?x)))
")
         (program (concatenate 'string
                               first-decl
                               second-decl
                               rule))
         (errors (validation-errors program)))
    (is (= 2 (length errors)))
    (let ((e1 (first errors))
          (e2 (second errors)))
      (is (typep e1 'lindalog:redundant-rd))
      (is (string= "(:rd (p ?x ?y))"
                   (covered-text (lindalog:semantic-error-ast e1) program)))
      (let ((related1 (lindalog:semantic-error-related e1)))
        (is (not (null related1)))
        (is (string= (string-trim '(#\Newline) first-decl)
                     (covered-text (car (first related1)) program))))
      (is (typep e2 'lindalog:redundant-rd))
      (is (string= "(:rd (q ?x))"
                   (covered-text (lindalog:semantic-error-ast e2) program)))
      (let ((related2 (lindalog:semantic-error-related e2)))
        (is (not (null related2)))
        (is (string= (string-trim '(#\Newline) second-decl)
                     (covered-text (car (first related2)) program)))))))

(test validate-program-rejects-errors-in-order
  (let* ((program "
(defpred p (a b)
  (:store :in))

(defrule foo
  (:when (p ?x))
  (:then (p ?x ?y ?z)))

(deffact (p 2 ?a))

(defrule bar
  (:when (:rd (q ?x))
         (r ?y))
  (:then (p ?x ?y)))

(defrule bar
  (:when (p 2 3)))

(defpred q (x)
  (:store :rd))

(defpred p (x)
  (:store :rd))
")
         (errors (validation-errors program)))
    (is (= 9 (length errors)))
    ;; 1. Duplicate predicate p
    (is (typep (first errors) 'lindalog:duplicate-predicate))
    (is (string= "(defpred p (x)
  (:store :rd))" (covered-text (lindalog:semantic-error-ast (first errors)) program)))
    (let ((related (lindalog:semantic-error-related (first errors))))
      (is (not (null related)))
      (is (string= "(defpred p (a b)
  (:store :in))"
                   (covered-text (car (first related)) program))))
    ;; 2. Duplicate rule bar
    (is (typep (second errors) 'lindalog:duplicate-rule))
    (is (string= "(defrule bar
  (:when (p 2 3)))" (covered-text (lindalog:semantic-error-ast (second errors)) program)))
    (let ((related (lindalog:semantic-error-related (second errors))))
      (is (not (null related)))
      (is (string= "(defrule bar
  (:when (:rd (q ?x))
         (r ?y))
  (:then (p ?x ?y)))" (covered-text (car (first related)) program))))
    ;; 3. Non-ground fact
    (is (typep (third errors) 'lindalog:variable-in-fact))
    (is (string= "?a" (covered-text (lindalog:semantic-error-ast (third errors)) program)))
    
    ;; 4. Arity mismatch (1)
    (is (typep (fourth errors) 'lindalog:arity-mismatch))
    (is (string= "(p ?x)" (covered-text (lindalog:semantic-error-ast (fourth errors)) program)))
    (let ((related (lindalog:semantic-error-related (fourth errors))))
      (is (not (null related)))
      (is (string= "(defpred p (a b)
  (:store :in))" (covered-text (car (first related)) program))))

    ;; 5. Arity mismatch (2)
    (is (typep (fifth errors) 'lindalog:arity-mismatch))
    (is (string= "(p ?x ?y ?z)" (covered-text (lindalog:semantic-error-ast (fifth errors)) program)))
    (let ((related (lindalog:semantic-error-related (fifth errors))))
      (is (not (null related)))
      (is (string= "(defpred p (a b)
  (:store :in))" (covered-text (car (first related)) program))))

    ;; 6. Unbound variable (1)
    (is (typep (sixth errors) 'lindalog:unbound-variable))
    (is (string= "?y" (covered-text (lindalog:semantic-error-ast (sixth errors)) program)))

    ;; 7. Unbound variable (2)
    (is (typep (seventh errors) 'lindalog:unbound-variable))
    (is (string= "?z" (covered-text (lindalog:semantic-error-ast (seventh errors)) program)))

    ;; 8. Redundant :rd
    (is (typep (eighth errors) 'lindalog:redundant-rd))
    (is (string= "(:rd (q ?x))" (covered-text (lindalog:semantic-error-ast (eighth errors)) program)))
    (let ((related (lindalog:semantic-error-related (eighth errors))))
      (is (not (null related)))
      (is (string= "(defpred q (x)
  (:store :rd))" (covered-text (car (first related)) program))))

    ;; 9. Undeclared predicate
    (is (typep (ninth errors) 'lindalog:undeclared-predicate))
    (is (string= "r" (covered-text (lindalog:semantic-error-ast (ninth errors)) program)))))

(test validate-program-rejects-duplicate-variable-in-init-fact
  (let* ((first-decl "
(defpred p (a b c)
  (:store :in))
")
         (fact "
(deffact (p ?x 2 ?x))
")
         (program (concatenate 'string
                               first-decl
                               fact))
         (errors (validation-errors program)))
    (is (= 2 (length errors)))
    (let ((e1 (first errors))
          (e2 (first errors)))
      (is (typep e1 'lindalog:variable-in-fact))
      (is (string= "?x" (covered-text (lindalog:semantic-error-ast e1) program)))
      (is (typep e2 'lindalog:variable-in-fact))
      (is (string= "?x" (covered-text (lindalog:semantic-error-ast e2) program))))))
