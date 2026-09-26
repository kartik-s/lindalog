;;;; parser.lisp

(in-package #:lindalog)

(defun variable-symbol-p (x)
  (and (symbolp x)
       (<= 2 (length (symbol-name x)))
       (equal (char (symbol-name x) 0)
              #\?)))

(defun parse-term (term context)
  (cond ((variable-symbol-p term)
         (make-variable term))
        ((and (symbolp term)
              (not (string= '? term))
              (not (keywordp term)))
         (make-constant term))
        ((integerp term)
         (make-constant term))
        (t (error 'syntax-error
                  :form context
                  :message (format nil "Invalid term: ~a"
                                   term)))))

(defun parse-atom (atom)
  "Parse a list into an atom AST node."
  (cond ((not (alexandria:proper-list-p atom))
         (error 'syntax-error
                :form atom
                :message (format nil "Atom is not a proper list")))
        ((null atom)
         (error 'syntax-error
                :form atom
                :message (format nil "Atom is an empty list")))
        (t
         (let ((predicate (first atom))
               (args (rest atom)))
           (cond ((not (symbolp predicate))
                  (error 'syntax-error
                         :form atom
                         :message (format nil "Atom predicate is not a symbol: ~s"
                                          predicate)))
                 ((member predicate '(defrule defpred rd)
                          :test #'string=)
                  (error 'syntax-error
                         :form atom
                         :message (format nil "Reserved symbol ~s used as an atom predicate"
                                          predicate)))
                 ((variable-symbol-p predicate)
                  (error 'syntax-error
                         :form atom
                         :message (format nil "Variable ~s used as an atom predicate"
                                          predicate)))
                 ((keywordp predicate)
                  (error 'syntax-error
                         :form atom
                         :message (format nil "Keyword ~s used as an atom predicate"
                                          predicate)))
                 ((null predicate)
                  (error 'syntax-error
                         :form atom
                         :message (format nil "NIL used as an atom predicate")))
                 (t (make-atom predicate (mapcar (lambda (arg)
                                                   (parse-term arg atom))
                                                 args))))))))
