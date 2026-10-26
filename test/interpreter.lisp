;;;; interpreter.lisp

(in-package :lindalog/tests)

(in-suite lindalog)

;;; FIRE-RULE

(defun database-matches-p (database unparsed-facts)
  (lindalog:multiset-equal-p
   (lindalog:all-fact-atoms database)
   (mapcar (lambda (form)
             (lindalog:parse-atom (as-cst form)))
           unparsed-facts)
   :test #'lindalog:ast-equal-p))

(test fire-rule-one-constant-premise
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p 1))
  (:then (p 2)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2))))))

(test fire-rule-two-constant-premises
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))
(deffact (p 2))

(defrule foo
  (:when (p 1) (p 2))
  (:then (p 3)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1) (p 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 3))))))

(test fire-rule-no-match
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p 3))
  (:then (p 4)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1))))))

(test fire-rule-variable-swap
  (let* ((image (make-test-image "
(defpred p (a b)
  (:store :in))

(deffact (p 1 2))

(defrule foo
  (:when (p ?x ?y))
  (:then (p ?y ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2 1))))))

(test fire-rule-two-rules
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p 1))
  (:then (p 2)))

(defrule bar
  (:when (p 2))
  (:then (p 3)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2))))
    (lindalog:fire-rule (second rules) image)
    (is (database-matches-p database '((p 3))))))

(test fire-rule-two-facts-same-var
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))
(deffact (p 1))
(deffact (p 2))

(defrule foo
  (:when (p ?x) (p ?x))
  (:then (p 3)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1) (p 1) (p 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (p 3))))))

(test fire-rule-two-facts-different-vars
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))
(deffact (p 1))
(deffact (p 2))

(defrule foo
  (:when (p ?x) (p ?y))
  (:then (p 3)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1) (p 1) (p 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (p 3))))))

(test fire-rule-no-then
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))
(deffact (p 2))

(defrule foo
  (:when (p ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1) (p 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2))))))

(test fire-rule-refract-rd-in
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :in))

(deffact (p 2))

(defrule foo
  (:when (:rd (p ?x)))
  (:then (q ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 2))))))

(test fire-rule-refract-sig
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :sig))

(deffact (q 2))

(defrule foo
  (:when (q ?y))
  (:then (p ?y)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((q 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 2))))))

(test fire-rule-refract-sig-and-rd-in
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :sig))

(defpred r (a b)
  (:store :in))

(deffact (p 2))
(deffact (q 3))

(defrule foo
  (:when (q ?y) (:rd (p ?x)))
  (:then (r ?x ?y)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 2) (q 3))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 3) (r 2 3))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 3) (r 2 3))))))

(test fire-rule-refract-rd
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :rd))

(defpred q (a)
  (:store :in))

(deffact (p 2))

(defrule foo
  (:when (p ?x))
  (:then (q ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 2))))))

(test fire-rule-refract-rd-and-rd-in
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :rd))

(defpred q (a)
  (:store :in))

(defpred r (a b)
  (:store :in))

(deffact (p 2))
(deffact (q 3))

(defrule foo
  (:when (p ?x) (:rd (q ?y)))
  (:then (r ?y ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 2) (q 3))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 3) (r 3 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 3) (r 3 2))))))

(test fire-rule-refract-rd-and-sig
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :rd))

(defpred q (a)
  (:store :sig))

(defpred r (a b)
  (:store :in))

(deffact (p 2))
(deffact (q 3))

(defrule foo
  (:when (p ?x) (q ?y))
  (:then (r ?y ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 2) (q 3))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 3) (r 3 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 3) (r 3 2))))))

(test fire-rule-refract-rd-and-sig-and-rd-in
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :rd))

(defpred q (a)
  (:store :sig))

(defpred r (a)
  (:store :in))

(defpred s (a b c)
  (:store :in))

(deffact (p 1))
(deffact (q 2))
(deffact (r 3))

(defrule foo
  (:when (p ?x) (q ?y) (:rd (r ?z)))
  (:then (s ?x ?y ?z)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1) (q 2) (r 3))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1) (q 2) (r 3) (s 1 2 3))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1) (q 2) (r 3) (s 1 2 3))))))

(test fire-rule-consume-and-read-fact
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p ?x) (:rd (p ?x)))
  (:then (q ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((q 1))))))

(test fire-rule-not-enough-facts
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a b)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p ?x) (p ?y))
  (:then (q ?x ?y)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1))))))

(test fire-rule-backtrack-match
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :in))

(deffact (p 1))
(deffact (p 2))

(defrule foo
  (:when (:rd (p ?x)))
  (:then (q ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1) (p 2))))
    (lindalog:fire-rule (first rules) image)
    ;; Facts are matched in insertion order
    (is (database-matches-p database '((p 1) (p 2) (q 1))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1) (p 2) (q 1) (q 2))))
    ;; Check that both activations are refracted
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1) (p 2) (q 1) (q 2))))))

(test fire-rule-refract-two-facts
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :in))

(deffact (p 1))
(deffact (p 1))

(defrule foo
  (:when (:rd (p ?x)))
  (:then (q ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 1) (p 1))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1) (p 1) (q 1))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1) (p 1) (q 1) (q 1))))
    ;; Check that both activations are refracted
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 1) (p 1) (q 1) (q 1))))))

(test fire-rule-replace-fact-different-ids
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p ?x))
  (:then (p ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (let ((first-id
            (lindalog:fact-id (aref (gethash 'lindalog-user::p
                                             (lindalog:database-facts-by-pred database))
                                    0))))
      (is (database-matches-p database '((p 1))))
      (lindalog:fire-rule (first rules) image)
      (is (database-matches-p database '((p 1))))
      (let ((second-id (lindalog:fact-id
                        (aref (gethash 'lindalog-user::p
                                       (lindalog:database-facts-by-pred database))
                              0))))
        (is (/= first-id second-id))))))

(test fire-rule-refract-two-rules-same-premise
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :rd))

(defpred q (a)
  (:store :in))

(deffact (p 2))

(defrule foo
  (:when (p ?x))
  (:then (q ?x)))

(defrule bar
  (:when (p ?x))
  (:then (q ?x)))
"))
         (database (lindalog:image-database image))
         (exe (lindalog:image-executable image))
         (program (lindalog:executable-program exe))
         (rules (lindalog:program-rules program)))
    (is (database-matches-p database '((p 2))))
    (lindalog:fire-rule (first rules) image)
    (is (database-matches-p database '((p 2) (q 2))))
    (lindalog:fire-rule (second rules) image)
    (is (database-matches-p database '((p 2) (q 2) (q 2))))
    ;; Check that both rules are refracted
    (lindalog:fire-rule (first rules) image)
    (lindalog:fire-rule (second rules) image)
    (is (database-matches-p database '((p 2) (q 2) (q 2))))))
