;;;; matching.lisp

(in-package :lindalog/tests)

(in-suite lindalog)

;;; MATCH-TERM

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

;;; MATCH-ATOM

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

;;; MATCH-PREMISES

(defun make-test-database (predicate-scs atoms)
  (let ((database (lindalog:make-database)))
    (dolist (pair predicate-scs)
      (lindalog::add-predicate (car pair) (cdr pair) database))
    (dolist (atom atoms)
      (lindalog::add-fact atom
                          (cdr (assoc (lindalog:atom-predicate atom)
                                      predicate-scs))
                          database))
    database))

(test match-premises-empty-premises-succeeds
  (let* ((database (lindalog::make-database))
         (match (lindalog::make-match))
         (new-match (lindalog::match-premises nil database match)))
    (is (eq lindalog::+no-bindings+
            (lindalog::match-bindings new-match)))))

(test match-premises-one-premise-succeeds
  (let ((database (make-test-database
                   '((p . :rd))
                   (list (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))))
        (premise (lindalog:make-premise
                  (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                  t)))
    (is (lindalog::bindings-equal-p (list (cons (lindalog:make-variable 'x)
                                                (lindalog:make-constant 'a)))
                                    (lindalog::match-bindings
                                     (lindalog::match-premises
                                      (list premise)
                                      database
                                      (lindalog::make-match)))))))

(test match-premises-one-premise-fails
  (let ((database (make-test-database
                   '((q . :rd))
                   (list (lindalog:make-atom 'q (list (lindalog:make-constant 'a))))))
        (premise (lindalog:make-premise
                  (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                  t)))
    (is (eq lindalog::+match-fail+
            (lindalog::match-premises
             (list premise)
             database
             (lindalog::make-match))))))

(test match-premises-shared-variable-succeeds
  (let ((database (make-test-database
                   '((p . :rd)
                     (q . :rd))
                   (list (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
                         (lindalog:make-atom 'q (list (lindalog:make-constant 'a))))))
        (premises (list (lindalog:make-premise
                         (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                         t)
                        (lindalog:make-premise
                         (lindalog:make-atom 'q (list (lindalog:make-variable 'x)))
                         t))))
    (is (lindalog::bindings-equal-p (list (cons (lindalog:make-variable 'x)
                                                (lindalog:make-constant 'a)))
                                    (lindalog::match-bindings
                                     (lindalog::match-premises
                                      premises
                                      database
                                      (lindalog::make-match)))))))

(test match-premises-shared-variable-inconsistent-fails
  (let ((database (make-test-database
                   '((p . :rd)
                     (q . :rd))
                   (list (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
                         (lindalog:make-atom 'q (list (lindalog:make-constant 'b))))))
        (premises (list (lindalog:make-premise
                         (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                         t)
                        (lindalog:make-premise
                         (lindalog:make-atom 'q (list (lindalog:make-variable 'x)))
                         t))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-premises
             premises
             database
             (lindalog::make-match))))))

(test match-premises-backtracks-to-earlier-premise
  (let ((database (make-test-database
                   '((p . :in)
                     (q . :in))
                   (list (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
                         (lindalog:make-atom 'p (list (lindalog:make-constant 'b)))
                         (lindalog:make-atom 'q (list (lindalog:make-constant 'b))))))
        (premises (list (lindalog:make-premise
                         (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                         t)
                        (lindalog:make-premise
                         (lindalog:make-atom 'q (list (lindalog:make-variable 'x)))
                         t))))
    (is (lindalog::bindings-equal-p (list (cons (lindalog:make-variable 'x)
                                                (lindalog:make-constant 'b)))
                                    (lindalog::match-bindings
                                     (lindalog::match-premises
                                      premises
                                      database
                                      (lindalog::make-match)))))))

(test match-premises-preserves-existing-bindings
  (let ((database (make-test-database
                   '((p . :in)
                     (q . :in))
                   (list (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))))
        (premises (list (lindalog:make-premise
                         (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                         t)))
        (match (lindalog::extend-match-bindings (lindalog::make-match)
                                                (list (cons (lindalog:make-variable 'x)
                                                            (lindalog:make-constant 'a))))))
    (is (lindalog::bindings-equal-p (list (cons (lindalog:make-variable 'x)
                                                (lindalog:make-constant 'a)))
                                    (lindalog::match-bindings
                                     (lindalog::match-premises
                                      premises
                                      database
                                      match))))))

(test match-premises-inconsistent-existing-binding-fails
  (let ((database (make-test-database
                   '((p . :in))
                   (list (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))))
        (premises (list (lindalog:make-premise
                         (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                         t)))
        (match (lindalog::extend-match-bindings (lindalog::make-match)
                                                (list (cons (lindalog:make-variable 'x)
                                                            (lindalog:make-constant 'b))))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-premises
             premises
             database
             match)))))

(test match-premises-consuming-in-premise-records-consumed-fact
  (let* ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
         (database (make-test-database
                   '((p . :in))
                   (list a)))
         (premise (lindalog:make-premise
                   (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                   nil)))
    (is (member a
                (lindalog::match-consumed-facts
                 (lindalog::match-premises
                  (list premise)
                  database
                  (lindalog::make-match)))
                :key #'lindalog::fact-atom
                :test #'lindalog:ast-equal-p))))

(test match-premises-read-only-in-premise-records-read-fact
  (let* ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
         (database (make-test-database
                   '((p . :in))
                   (list a)))
         (premise (lindalog:make-premise
                   (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                   t)))
    (is (member a
                (lindalog::match-read-facts
                 (lindalog::match-premises
                  (list premise)
                  database
                  (lindalog::make-match)))
                :key #'lindalog::fact-atom
                :test #'lindalog:ast-equal-p))))

(test match-premises-rd-premise-records-read-fact
  (let* ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
         (database (make-test-database
                   '((p . :rd))
                   (list a)))
         (premise (lindalog:make-premise
                   (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                   t)))
    (is (member a
                (lindalog::match-read-facts
                 (lindalog::match-premises
                  (list premise)
                  database
                  (lindalog::make-match)))
                :key #'lindalog::fact-atom
                :test #'lindalog:ast-equal-p))))

(test match-premises-sub-premise-records-read-fact
  (let* ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
         (database (make-test-database
                   '((p . :sub))
                   (list a)))
         (premise (lindalog:make-premise
                   (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                   t)))
    (is (member a
                (lindalog::match-read-facts
                 (lindalog::match-premises
                  (list premise)
                  database
                  (lindalog::make-match)))
                :key #'lindalog::fact-atom
                :test #'lindalog:ast-equal-p))))

(test match-premises-two-consuming-premises-cannot-reuse-fact
  (let ((database (make-test-database
                   '((p . :in))
                   (list (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))))
        (premises (list (lindalog:make-premise
                         (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                         nil)
                        (lindalog:make-premise
                         (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                         nil))))
    (is (eq lindalog::+match-fail+
            (lindalog::match-premises
             premises
             database
             (lindalog::make-match))))))

(test match-premises-two-consuming-premises-can-use-distinct-equal-facts
  (let* ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
         (database (make-test-database
                    '((p . :in))
                    (list a a)))
         (premises (list (lindalog:make-premise
                          (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                          nil)
                         (lindalog:make-premise
                          (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                          nil))))
    (is (= 2 (count a
                    (lindalog::match-consumed-facts
                     (lindalog::match-premises
                      premises
                      database
                      (lindalog::make-match)))
                    :key #'lindalog::fact-atom
                    :test #'lindalog:ast-equal-p)))))

(test match-premises-rejected-match-fails
  (let ((database (make-test-database
                   '((p . :rd))
                   (list (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))))
        (premise (lindalog:make-premise
                  (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                  t)))
    (is (eq lindalog::+match-fail+
            (lindalog::match-premises
             (list premise)
             database
             (lindalog::make-match)
             (constantly nil))))))

(test match-premises-backtracks-after-rejected-match
  (let* ((b (lindalog:make-atom 'p (list (lindalog:make-constant 'b))))
         (database (make-test-database
                    '((p . :in))
                    (list
                     (lindalog:make-atom 'p (list (lindalog:make-constant 'a)))
                     b
                     (lindalog:make-atom 'p (list (lindalog:make-constant 'c))))))
         (premise (lindalog:make-premise
                   (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                   nil)))
    (is (member b
                (lindalog::match-consumed-facts
                 (lindalog::match-premises
                  (list premise)
                  database
                  (lindalog::make-match)
                  (lambda (match)
                    (some (lambda (fact)
                            (eq (lindalog:constant-value
                                 (first (lindalog:atom-args
                                         (lindalog::fact-atom fact))))
                                'b))
                          (lindalog::match-consumed-facts match)))))
                :key #'lindalog::fact-atom
                :test #'lindalog:ast-equal-p))))

(test match-premises-read-and-consuming-premises-can-share-in-fact
    (let* ((a (lindalog:make-atom 'p (list (lindalog:make-constant 'a))))
           (database (make-test-database
                      '((p . :in))
                      (list a)))
           (premises (list (lindalog:make-premise
                            (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                            nil)
                           (lindalog:make-premise
                            (lindalog:make-atom 'p (list (lindalog:make-variable 'x)))
                            t)))
           (match (lindalog::match-premises
                        premises
                        database
                        (lindalog::make-match))))
      (is (and (member a (lindalog::match-consumed-facts match)
                       :key #'lindalog::fact-atom
                       :test #'lindalog:ast-equal-p)
               (member a (lindalog::match-read-facts match)
                       :key #'lindalog::fact-atom
                       :test #'lindalog:ast-equal-p)))))
