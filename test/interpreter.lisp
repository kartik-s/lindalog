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

;;; REACT

(test react-no-activations
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p 2))
  (:then (p 3)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((p 1))))
    (lindalog:react image '())
    (is (database-matches-p database '((p 1))))))

(test react-one-activation
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p 1))
  (:then (p 2)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((p 1))))
    (lindalog:react image '())
    (is (database-matches-p database '((p 2))))
    (lindalog:react image '())
    (is (database-matches-p database '((p 2))))))

(test react-two-activations
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :in))

(defpred r (a)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p ?x))
  (:then (q ?x)))

(defrule bar
  (:when (q ?x))
  (:then (r ?x)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((p 1))))
    (lindalog:react image '())
    (is (database-matches-p database '((r 1))))
    (lindalog:react image '())
    (is (database-matches-p database '((r 1))))))

(test react-signal-lasts-one-cycle
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :sig))

(defpred r (a b)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p ?x) (q ?y))
  (:then (r ?x ?y)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((p 1))))
    (lindalog:react image (list (lindalog:make-atom
                                 (lindalog:make-pred-ref 'lindalog-user::q)
                                 (list (lindalog:make-constant 2)))))
    (is (database-matches-p database '((r 1 2))))
    ;; The signal should be gone here
    (lindalog:react image (list (lindalog:make-atom
                                 (lindalog:make-pred-ref 'lindalog-user::p)
                                 (list (lindalog:make-constant 2)))))
    (is (database-matches-p database '((p 2) (r 1 2))))))

(test react-declared-signal-not-asserted
  (let* ((image (make-test-image "
(defpred p (a)
  (:store :in))

(defpred q (a)
  (:store :sig))

(defpred r (a b)
  (:store :in))

(deffact (p 1))

(defrule foo
  (:when (p ?x) (q ?y))
  (:then (r ?x ?y)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((p 1))))
    (lindalog:react image '())
    (is (database-matches-p database '((p 1))))))

(test react-restarts-from-top
  (let* ((image (make-test-image "
(defpred a ()
  (:store :in))

(defpred b ()
  (:store :in))

(defpred z ()
  (:store :in))

(defpred win1 ()
  (:store :in))

(defpred win3 ()
  (:store :in))

(deffact (a))
(deffact (z))

(defrule r1
  (:when (b) (z))
  (:then (win1)))

(defrule r2
  (:when (a))
  (:then (b)))

(defrule r3
  (:when (b) (z))
  (:then (win3)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((a) (z))))
    (lindalog:react image '())
    (is (database-matches-p database '((win1))))))

(test react-first-rule-fires
  (let* ((image (make-test-image "
(defpred a ()
  (:store :in))


(defpred win1 ()
  (:store :in))

(defpred win2 ()
  (:store :in))

(deffact (a))

(defrule r1
  (:when (a))
  (:then (win1)))

(defrule r2
  (:when (a))
  (:then (win2)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((a))))
    (lindalog:react image '())
    (is (database-matches-p database '((win1))))))

(test react-ping-pong
  (let* ((image (make-test-image "
(defpred ping ()
  (:store :in))

(defpred pong ()
  (:store :sig))

(defpred win ()
  (:store :in))

(deffact (ping))

(defrule r1
  (:when (ping))
  (:then (pong)))

(defrule r2
  (:when (pong))
  (:then (win)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((ping))))
    (lindalog:react image '())
    (is (database-matches-p database '((win))))
    (lindalog:react image '())
    (is (database-matches-p database '((win))))))

(test react-signal-twice
  (let* ((image (make-test-image "
(defpred p ()
  (:store :in))

(defpred q ()
  (:store :sig))

(defrule foo
  (:when (q))
  (:then (p)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '()))
    (lindalog:react image (list (lindalog:make-atom
                                 (lindalog:make-pred-ref 'lindalog-user::q)
                                 '())))
    (is (database-matches-p database '((p))))
    (lindalog:react image (list (lindalog:make-atom
                                 (lindalog:make-pred-ref 'lindalog-user::q)
                                 '())))
    (is (database-matches-p database '((p) (p))))
    (lindalog:react image '())
    (is (database-matches-p database '((p) (p))))))

(test react-rd-rule-fires-once
  (let* ((image (make-test-image "
(defpred p ()
  (:store :rd))

(defpred q ()
  (:store :in))

(defpred r ()
  (:store :in))

(deffact (p))
(deffact (q))

(defrule foo
  (:when (p) (:rd (q)))
  (:then (r)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((p) (q))))
    (lindalog:react image '())
    (is (database-matches-p database '((p) (q) (r))))
    (dotimes (x 5)
      (lindalog:react image '())
      (is (database-matches-p database '((p) (q) (r)))))))

(test react-rd-rule-fires-before-consuming-rule
  (let* ((image (make-test-image "
(defpred a ()
  (:store :in))

(defpred b ()
  (:store :in))

(defpred c ()
  (:store :in))

(deffact (a))

(defrule c1
  (:when (a))
  (:then (c)))

(defrule b1
  (:when (:rd (a)))
  (:then (b)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((a))))
    (lindalog:react image '())
    (is (database-matches-p database '((b) (c))))
    (lindalog:react image '())
    (is (database-matches-p database '((b) (c))))))

(test react-rd-rules-sort-stably
  (let* ((image (make-test-image "
(defpred x (a)
  (:store :in))

(defpred y ()
  (:store :rd))

(defpred got (a)
  (:store :in))

(deffact (y))

(defrule r2
  (:when (y))
  (:then (x 2)))

(defrule r3
  (:when (x ?a) (x ?b))
  (:then (got ?a)))

(defrule r1
  (:when (y))
  (:then (x 1)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((y))))
    (lindalog:react image '())
    (is (database-matches-p database '((y) (got 2))))
    (lindalog:react image '())
    (is (database-matches-p database '((y) (got 2))))))

(test react-consuming-rules-sort-stably
  (let* ((image (make-test-image "
(defpred x ()
  (:store :in))

(defpred y (a)
  (:store :in))

(deffact (x))

(defrule r2
  (:when (x))
  (:then (y 2)))

(defrule r3
  (:when (:rd (x)))
  (:then (y 3)))

(defrule r1
  (:when (x))
  (:then (y 1)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((x))))
    (lindalog:react image '())
    (is (database-matches-p database '((y 2) (y 3))))
    (lindalog:react image '())
    (is (database-matches-p database '((y 2) (y 3))))))

(test react-rd-rule-fires-between-two-consuming-rules
  (let* ((image (make-test-image "
(defpred x (a)
  (:store :in))

(deffact (x 1))

(defrule c2
  (:when (x 2))
  (:then (x 4)))

(defrule r1
  (:when (:rd (x 2)))
  (:then (x 3)))

(defrule c1
  (:when (x 1))
  (:then (x 2)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((x 1))))
    (lindalog:react image '())
    (is (database-matches-p database '((x 3) (x 4))))
    (lindalog:react image '())
    (is (database-matches-p database '((x 3) (x 4))))))

(test react-sig-rule-fires-before-consuming-rule
  (let* ((image (make-test-image "
(defpred x (a)
  (:store :in))

(defpred y ()
  (:store :sig))

(deffact (x 1))

(defrule r2
  (:when (x 1))
  (:then (x 3)))

(defrule r1
  (:when (:rd (x 1)) (y))
  (:then (x 2)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((x 1))))
    (lindalog:react image (list (lindalog:make-atom
                                 (lindalog:make-pred-ref 'lindalog-user::y)
                                 '())))
    (is (database-matches-p database '((x 2) (x 3))))
    (lindalog:react image '())
    (is (database-matches-p database '((x 2) (x 3))))))

(test react-combust
  (let* ((image (make-test-image "
(defpred h2 ()
  (:store :in))

(defpred o2 ()
  (:store :in))

(defpred h2o ()
  (:store :in))

(defpred spark ()
  (:store :sig))

(deffact (h2))
(deffact (h2))
(deffact (h2))
(deffact (o2))

(defrule combust
  (:when (spark) (h2) (h2) (o2))
  (:then (h2o)))
"))
         (database (lindalog:image-database image)))
    (is (database-matches-p database '((h2) (h2) (h2) (o2))))
    (lindalog:react image '())
    (is (database-matches-p database '((h2) (h2) (h2) (o2))))
    (lindalog:react image (list (lindalog:make-atom
                                 (lindalog:make-pred-ref 'lindalog-user::spark)
                                 '())))
    (is (database-matches-p database '((h2) (h2o))))
    ;; No spark, no reaction
    (lindalog:react image (list (lindalog:make-atom
                                 (lindalog:make-pred-ref 'lindalog-user::h2)
                                 '())
                                (lindalog:make-atom
                                 (lindalog:make-pred-ref 'lindalog-user::o2)
                                 '())))
    (is (database-matches-p database '((h2) (h2) (o2) (h2o))))))
