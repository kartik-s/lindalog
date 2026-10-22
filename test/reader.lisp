;;;; reader.lisp

(in-package #:lindalog/tests)

(in-suite lindalog)

;;; READ-PROGRAM

(test read-program-empty-stream
  (is (null (with-input-from-string (stream "")
              (lindalog:read-program stream)))))

(test read-program-only-whitespace
  (is (null (with-input-from-string (stream (format nil "   ~%   ~C  ~%~C"
                                                    #\Tab #\Tab))
              (lindalog:read-program stream)))))

(test read-program-only-comments
  (is (null (with-input-from-string (stream (format nil "; foo~%#|bar|#~%"))
              (lindalog:read-program stream)))))

(defun form-equal-p (a b)
  "Check if forms A and B are equal, comparing symbols by name."
  (tree-equal a b :test (lambda (x y)
                          (if (and (symbolp x) (symbolp y))
                              (string= x y)
                              (eql x y)))))

(test read-program-one-form
  (let ((forms (with-input-from-string (stream (format nil "
(defpred p (a b)
  (:store :in))"))
                 (lindalog:read-program stream))))
    (is (= 1 (length forms)))
    (is (form-equal-p
         '(defpred p (a b) (:store :in))
         (cst:raw (first forms))))))

(test read-program-three-forms
  (let ((forms (with-input-from-string (stream (format nil "
(defpred p (a b)
  (:store :in))

(deffact (p 1 a))

(defrule foo
  (:when (p ?x ?y))
  (:then (p ?y ?x)))"))
                 (lindalog:read-program stream))))
    (is (= 3 (length forms)))
    (is (form-equal-p
         '(defpred p (a b) (:store :in))
         (cst:raw (first forms))))
    (is (form-equal-p
         '(deffact (p 1 a))
         (cst:raw (second forms))))
    (is (form-equal-p
         '(defrule foo
           (:when (p ?x ?y))
           (:then (p ?y ?x)))
         (cst:raw (third forms)))))

  (test read-program-three-forms-with-comments
    (let ((forms (with-input-from-string (stream (format nil "
;; predicate
(defpred p (a b)
  (:store :in))

;; ground fact
(deffact (p 1 a))

;; rule
(defrule foo
  (:when (p ?x ?y))
  (:then (p ?y ?x)))"))
                   (lindalog:read-program stream))))
      (is (= 3 (length forms)))
      (is (form-equal-p
           '(defpred p (a b) (:store :in))
           (cst:raw (first forms))))
      (is (form-equal-p
           '(deffact (p 1 a))
           (cst:raw (second forms))))
      (is (form-equal-p
           '(defrule foo
             (:when (p ?x ?y))
             (:then (p ?y ?x)))
           (cst:raw (third forms)))))))

(test read-program-source-ranges
  (let ((forms (with-input-from-string (stream "
(defpred q (a)
  (:store :in))

(deffact (q 2))")
                 (lindalog:read-program stream))))
    (is (= 2 (length forms)))
    (is (equal '(1 . 31) (cst:source (first forms))))
    (is (equal '(33 . 48) (cst:source (second forms))))))

(test read-program-symbols-interned-in-lindalog-user
  (let ((forms (with-input-from-string (stream "a")
                 (lindalog:read-program stream))))
    (is (= 1 (length forms)))
    (is (eq (find-package '#:lindalog-user)
            (symbol-package (cst:raw (first forms)))))))

(test read-program-keywords-interned-in-keyword
  (let ((forms (with-input-from-string (stream ":store")
                 (lindalog:read-program stream))))
    (is (= 1 (length forms)))
    (is (eq (find-package '#:keyword)
            (symbol-package (cst:raw (first forms)))))))

(test read-program-rejects-missing-paren
  (handler-case
      (progn
        (with-input-from-string (stream "(a b")
          (lindalog:read-program stream))
        (fail "expected a READ-ERROR"))
    (lindalog:read-error (e)
      (is (equal '(4 . 4) (lindalog:read-error-range e))))))

(test read-program-rejects-extra-paren
  (handler-case
      (progn
        (with-input-from-string (stream "(a b))")
          (lindalog:read-program stream))
        (fail "expected a READ-ERROR"))
    (lindalog:read-error (e)
      (is (equal '(5 . 6) (lindalog:read-error-range e))))))

(test read-program-rejects-unterminated-string
  (handler-case
      (progn
        (with-input-from-string (stream "\"docstring")
          (lindalog:read-program stream))
        (fail "expected a READ-ERROR"))
    (lindalog:read-error (e)
      (is (equal '(10 . 10) (lindalog:read-error-range e))))))

(test read-program-rejects-package-syntax
  (handler-case
      (progn
        (with-input-from-string (stream "foo:bar")
          (lindalog:read-program stream))
        (fail "expected a READ-ERROR"))
    (lindalog:read-error (e)
      (is (equal '(0 . 7) (lindalog:read-error-range e)))
      (is (string= "[0-7]: package syntax not allowed: FOO:BAR"
                   (princ-to-string e))))))

(defvar *rejects-reader-macro-var* 0)

(test read-program-rejects-reader-macro
  (signals lindalog:read-error
    (with-input-from-string (stream "#.(setf *rejects-reader-macro-var* 1")
      (lindalog:read-program stream)))
  (is (= 0 *rejects-reader-macro-var*)))

(test read-program-rejects-circuluar-input
  (signals lindalog:read-error
    (with-input-from-string (stream "#1=(a . #1#)")
      (lindalog:read-program stream))))
