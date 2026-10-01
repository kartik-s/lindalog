;;;; parser.lisp

(in-package #:lindalog)

(defun variable-symbol-p (x)
  (and (symbolp x)
       (not (keywordp x))
       (<= 2 (length (symbol-name x)))
       (equal (char (symbol-name x) 0)
              #\?)))

(defun parse-term (form context)
  (cond ((null form)
         (error 'syntax-error
                :form context
                :message "term cannot be nil"))
        ((variable-symbol-p form)
         (make-variable form))
        ((and (symbolp form)
              (not (string= '? form))
              (not (keywordp form)))
         (make-constant form))
        ((integerp form)
         (make-constant form))
        (t (error 'syntax-error
                  :form context
                  :message (format nil "invalid term: ~a"
                                   form)))))

(defun parse-atom (form)
  "Parse a list into an atom AST node."
  (cond ((not (alexandria:proper-list-p form))
         (error 'syntax-error
                :form form
                :message (format nil "atom is not a proper list")))
        ((null form)
         (error 'syntax-error
                :form form
                :message (format nil "atom is an empty list")))
        (t
         (let ((predicate (first form))
               (args (rest form)))
           (cond ((not (symbolp predicate))
                  (error 'syntax-error
                         :form form
                         :message (format nil "atom predicate is not a symbol: ~s"
                                          predicate)))
                 ((or (member predicate '(defrule defpredicate rd)
                              :test #'string=)
                      (eq :rd predicate))
                  (error 'syntax-error
                         :form form
                         :message (format nil "reserved symbol used as an atom predicate: ~s"
                                          predicate)))
                 ((variable-symbol-p predicate)
                  (error 'syntax-error
                         :form form
                         :message (format nil "variable used as an atom predicate: ~s"
                                          predicate)))
                 ((keywordp predicate)
                  (error 'syntax-error
                         :form form
                         :message (format nil "keyword used as an atom predicate: ~s"
                                          predicate)))
                 ((null predicate)
                  (error 'syntax-error
                         :form form
                         :message (format nil "NIL used as an atom predicate")))
                 (t (make-atom predicate (mapcar (lambda (arg)
                                                   (parse-term arg form))
                                                 args))))))))

(defun parse-premise (form)
  "Parse a list into a premise AST node."
  (cond ((not (alexandria:proper-list-p form))
         (error 'syntax-error
                :form form
                :message "premise must either be (:rd <atom>) or <atom>"))
        ((null form)
         (error 'syntax-error
                :form form
                :message "premise is NIL"))
        ((eq :rd (first form))
         (if (/= 2 (length form))
             (error 'syntax-error
                    :form form
                    :message ":rd premise must be of the form (:rd <atom>)")
             (make-premise (parse-atom (second form)) t)))
        ((and (symbolp (first form))
              (string= 'rd (first form)))
         (error 'syntax-error
                :form form
                :message (format nil  "use :rd to mark a read-only-premise: ~s" form)))
        (t (make-premise (parse-atom form) nil))))

(defun parse-conclusion (form)
  "Parse a list into an atom AST node, checking that :rd is not used."
  (cond ((and (alexandria:proper-list-p form)
              (eq :rd (first form)))
         (error 'syntax-error
                :form form
                :message ":rd can only be used in premises"))
        (t (parse-atom form))))
