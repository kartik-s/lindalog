;;;; parser.lisp

(in-package #:lindalog)

(defun variable-symbol-p (x)
  "Check if X is a non-keyword symbol whose name is a question mark
followed by at least one more character."
  (and (symbolp x)
       (not (keywordp x))
       (<= 2 (length (symbol-name x)))
       (equal (char (symbol-name x) 0)
              #\?)))

(defparameter *reserved-names* '(defrule defpred deffact rd in sub))
(defparameter *reserved-keywords* '(:rd :in :sub))

(defun reserved-name-p (name)
  (member name *reserved-names*
          :test #'string=))

(defun reserved-keyword-p (keyword)
  (member keyword *reserved-keywords*
          :test #'eq))

(defun parse-term (form context)
  "Parse FORM into a constant or variable AST node, signaling
SYNTAX-ERROR if FORM is malformed."
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
  "Parse FORM into an atom AST node, signaling SYNTAX-ERROR if FORM is
malformed."
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
                 ((or (reserved-name-p predicate)
                      (reserved-keyword-p predicate))
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
  "Parse FORM into a premise AST node, signaling SYNTAX-ERROR if FORM is
malformed."
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
  "Parse FORM into an atom AST node, signaling SYNTAX-ERROR if FORM is
malformed or marked :RD."
  (cond ((and (alexandria:proper-list-p form)
              (eq :rd (first form)))
         (error 'syntax-error
                :form form
                :message ":rd can only be used in premises"))
        (t (parse-atom form))))

(defun parse-pred-decl (form)
  "Parse FORM into a PRED-DECL AST node, signaling SYNTAX-ERROR if FORM
is malformed."
  (if (or (not (alexandria:proper-list-p form))
          (/= 4 (length form)))
      (error 'syntax-error
             :form form
             :message "predicate declaration must be of the form (defpred <name> <store> (<arg-name>*))")
      (let ((name (second form))
            (store (third form))
            (args (fourth form)))
        (cond ((null name)
               (error 'syntax-error
                      :form form
                      :message "predicate name is NIL"))
              ((not (alexandria:proper-list-p args))
               (error 'syntax-error
                      :form form
                      :message "predicate arguments must be a proper list"))
              ((or (not (symbolp name))
                   (variable-symbol-p name)
                   (keywordp name))
               (error 'syntax-error
                      :form form
                      :message "predicate name cannot be a variable, a keyword, or a non-symbol"))
              ((reserved-name-p name)
               (error 'syntax-error
                      :form form
                      :message (format nil "predicate name is reserved: ~s" name)))
              ((not (member store '(:rd :in :sub)))
               (error 'syntax-error
                      :form form
                      :message (format nil "predicate store must be one of :rd, :in, or :sub, got: ~s"
                                       store)))
              (t (loop :with parsed-args := nil
                       :for arg :in args
                       :do (cond ((or (not (symbolp arg))
                                      (variable-symbol-p arg)
                                      (keywordp arg))
                                  (error 'syntax-error
                                         :form form
                                         :message "predicate argument cannot be a variable, a keyword, or a non-symbol"))
                                 ((reserved-name-p arg)
                                  (error 'syntax-error
                                         :form form
                                         :message (format nil "predicate argument is reserved: ~s" arg)))
                                 ((member arg parsed-args :test #'string=)
                                  (error 'syntax-error
                                         :form form
                                         :message (format nil "duplicate predicate argument: ~s" arg)))
                                 (t (push arg parsed-args)))
                       :finally (return (make-pred-decl name store (nreverse parsed-args)))))))))
