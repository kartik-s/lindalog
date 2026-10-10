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
  "Check if the symbol NAME is a reserved symbol."
  (member name *reserved-names*
          :test #'string=))

(defun reserved-keyword-p (keyword)
  "Check if the keyword KEYWORD is a reserved keyword."
  (member keyword *reserved-keywords*
          :test #'eq))

(defun cst-headed-by-p (cst keyword)
  "Check if CST is a list whose first element is KEYWORD."
  (and (cst:consp cst)
       (eq keyword (cst:raw (cst:first cst)))))

(defun parse-term (cst)
  "Parse CST into a CONSTANT or VARIABLE AST node, signaling
SYNTAX-ERROR if CST is malformed."
  (let ((form (cst:raw cst)))
    (cond ((cst:null cst)
           (error 'syntax-error
                  :cst cst
                  :message "term cannot be nil"))
          ((variable-symbol-p form)
           (make-variable form :cst cst))
          ((and (symbolp form)
                (not (string= '? form))
                (not (keywordp form)))
           (make-constant form :cst cst))
          ((integerp form)
           (make-constant form :cst cst))
          (t (error 'syntax-error
                    :cst cst
                    :message (format nil "invalid term: ~s"
                                     form))))))

(defun parse-atom (cst)
  "Parse CST into an ATOM AST node, signaling SYNTAX-ERROR if CST is
malformed."
  (let ((parts (and (cst:proper-list-p cst)
                    (cst:listify cst))))
    (cond ((not parts)
           (error 'syntax-error
                  :cst cst
                  :message (format nil "atom is not a proper list")))
          ((cst:null cst)
           (error 'syntax-error
                  :cst cst
                  :message (format nil "atom is an empty list")))
          (t (let* ((predicate-cst (first parts))
                    (predicate (cst:raw predicate-cst))
                    (args-cst (rest parts)))
               (cond ((not (symbolp predicate))
                      (error 'syntax-error
                             :cst predicate-cst
                             :message (format nil "atom predicate is not a symbol: ~s"
                                              predicate)))
                     ((or (reserved-name-p predicate)
                          (reserved-keyword-p predicate))
                      (error 'syntax-error
                             :cst predicate-cst
                             :message (format nil "reserved symbol used as an atom predicate: ~s"
                                              predicate)))
                     ((variable-symbol-p predicate)
                      (error 'syntax-error
                             :cst predicate-cst
                             :message (format nil "variable used as an atom predicate: ~s"
                                              predicate)))
                     ((keywordp predicate)
                      (error 'syntax-error
                             :cst predicate-cst
                             :message (format nil "keyword used as an atom predicate: ~s"
                                              predicate)))
                     ((null predicate)
                      (error 'syntax-error
                             :cst predicate-cst
                             :message (format nil "NIL used as an atom predicate")))
                     (t (make-atom predicate (mapcar (lambda (arg)
                                                       (parse-term arg))
                                                     args-cst)
                                   :cst cst))))))))

(defun parse-premise (cst)
  "Parse CST into a PREMISE AST node, signaling SYNTAX-ERROR if CST is
malformed."
  (let ((parts (and (cst:proper-list-p cst)
                    (cst:listify cst))))
    (cond ((not parts)
           (error 'syntax-error
                  :cst cst
                  :message "premise must either be (:rd <atom>) or <atom>"))
          ((cst-headed-by-p cst :rd)
           (if (/= 2 (length parts))
               (error 'syntax-error
                      :cst cst
                      :message ":rd premise must be of the form (:rd <atom>)")
               (make-premise (parse-atom (second parts)) t :cst cst)))
          ((and (symbolp (cst:raw (first parts)))
                (string= 'rd (cst:raw (first parts))))
           (error 'syntax-error
                  :cst (first parts)
                  :message (format nil  "use :rd to mark a read-only premise: ~s"
                                   (cst:raw (first parts)))))
          (t (make-premise (parse-atom cst) nil :cst cst)))))

(defun parse-conclusion (cst)
  "Parse CST into an ATOM AST node, signaling SYNTAX-ERROR if CST is
malformed or marked :RD."
  (cond ((cst-headed-by-p cst :rd)
         (error 'syntax-error
                :cst cst
                :message ":rd can only be used in premises"))
        (t (parse-atom cst))))

(defun parse-pred-decl (cst)
  "Parse CST into a PRED-DECL AST node, signaling SYNTAX-ERROR if CST
is malformed."
  (let ((parts (and (cst:proper-list-p cst)
                    (cst:listify cst))))
    (if (/= 4 (length parts))
        (error 'syntax-error
               :cst cst
               :message "predicate declaration must be of the form (defpred <name> <store> (<arg-name>*))")
        (let* ((name-cst (second parts))
               (name (cst:raw name-cst))
               (store-cst (third parts))
               (args-cst (fourth parts)))
          (cond ((cst:null name-cst)
                 (error 'syntax-error
                        :cst name-cst
                        :message "predicate name is NIL"))
                ((or (not (symbolp name))
                     (variable-symbol-p name)
                     (keywordp name))
                 (error 'syntax-error
                        :cst name-cst
                        :message "predicate name cannot be a variable, a keyword, or a non-symbol"))
                ((reserved-name-p name)
                 (error 'syntax-error
                        :cst name-cst
                        :message (format nil "predicate name is reserved: ~s" name)))
                ((not (member (cst:raw store-cst) '(:rd :in :sub)))
                 (error 'syntax-error
                        :cst store-cst
                        :message (format nil "expected :rd, :in, or :sub for predicate store, got: ~s"
                                         (cst:raw store-cst))))
                ((not (cst:proper-list-p args-cst))
                 (error 'syntax-error
                        :cst args-cst
                        :message "predicate arguments must be a proper list"))
                (t (loop :with parsed-args := nil
                         :for arg-cst :in (cst:listify args-cst)
                         :for arg := (cst:raw arg-cst)
                         :do (cond ((or (not (symbolp arg))
                                        (variable-symbol-p arg)
                                        (keywordp arg))
                                    (error 'syntax-error
                                           :cst arg-cst
                                           :message "predicate argument cannot be a variable, a keyword, or a non-symbol"))
                                   ((reserved-name-p arg)
                                    (error 'syntax-error
                                           :cst arg-cst
                                           :message (format nil "predicate argument is reserved: ~s" arg)))
                                   ((member arg parsed-args :test #'string=)
                                    (error 'syntax-error
                                           :cst arg-cst
                                           :message (format nil "duplicate predicate argument: ~s" arg)))
                                   (t (push arg parsed-args)))
                         :finally (return (make-pred-decl name (cst:raw store-cst) (nreverse parsed-args) :cst cst)))))))))

(defun parse-init-fact (cst)
  "Parse CST into an INIT-FACT AST node, signaling SYNTAX-ERROR if CST
is malformed."
  (let ((parts (and (cst:proper-list-p cst)
                    (cst:listify cst))))
    (if (/= 2 (length parts))
        (error 'syntax-error
               :cst cst
               :message "deffact must be of the form (deffact <atom>)")
        (let* ((atom (parse-atom (second parts)))
               (vars (atom-variables atom)))
          (if (null vars)
              (make-init-fact atom :cst cst)
              (error 'syntax-error
                     :cst (ast-node-cst (first vars))
                     :message (format nil "fact atom contains a variable and is therefore not ground: ~s"
                                      (variable-name (first vars)))))))))

(defun parse-rule (cst)
  "Parse CST into a RULE AST node, signaling SYNTAX-ERROR if CST is
malformed."
  (let ((parts (and (cst:proper-list-p cst)
                    (cst:listify cst))))
    (if (not (member (length parts) '(4 5)))
        (error 'syntax-error
               :cst cst
               :message "rule must be of the form (defrule <name> <docstring>? (:when <atom>+) (:then <atom>*))")
        (let* ((docstring-p (= 5 (length parts)))
               (name-cst (second parts))
               (name (cst:raw name-cst))
               (docstring-cst (and docstring-p
                                   (third parts)))
               (lhs-cst (if docstring-p
                            (fourth parts)
                            (third parts)))
               (rhs-cst (if docstring-p
                            (fifth parts)
                            (fourth parts))))
          (cond ((and docstring-p
                      (cst-headed-by-p docstring-cst :when))
                 (let ((extra-cst (if (cst-headed-by-p lhs-cst :then)
                                      rhs-cst
                                      lhs-cst)))
                   (error 'syntax-error
                          :cst extra-cst
                          :message (format nil "extraneous form in rule: ~s"
                                           (cst:raw extra-cst)))))
                ((null name)
                 (error 'syntax-error
                        :cst name-cst
                        :message "rule name is NIL"))
                ((or (not (symbolp name))
                     (variable-symbol-p name)
                     (keywordp name))
                 (error 'syntax-error
                        :cst name-cst
                        :message "rule name cannot be a variable, a keyword, or a non-symbol"))
                ((reserved-name-p name)
                 (error 'syntax-error
                        :cst name-cst 
                        :message (format nil "rule name is reserved: ~s" name)))
                ((and docstring-p
                      (not (stringp (cst:raw docstring-cst))))
                 (error 'syntax-error
                        :cst docstring-cst
                        :message (format nil "docstring is not a string: ~s"
                                         (cst:raw docstring-cst))))
                (t (let ((lhs (and (cst:proper-list-p lhs-cst)
                                   (cst:listify lhs-cst)))
                         (rhs (and (cst:proper-list-p rhs-cst)
                                   (cst:listify rhs-cst))))
                     (cond ((not lhs)
                            (error 'syntax-error
                                   :cst lhs-cst
                                   :message "lhs of rule must be of the form (:when <atom>+)"))
                           ((not (cst-headed-by-p lhs-cst :when))
                            (error 'syntax-error
                                   :cst (first lhs)
                                   :message (format nil "expected :when, got: ~s"
                                                    (cst:raw (first lhs)))))
                           ((not (rest lhs))
                            (error 'syntax-error
                                   :cst lhs-cst
                                   :message ":when clause has no premises"))
                           ((not rhs)
                            (error 'syntax-error
                                   :cst rhs-cst
                                   :message "rhs of rule must be of the form (:then <atom>*)"))
                           ((not (cst-headed-by-p rhs-cst :then))
                            (error 'syntax-error
                                   :cst (first rhs)
                                   :message (format nil "expected :then, got: ~s"
                                                    (cst:raw (first rhs)))))
                           (t (make-rule
                               name
                               (mapcar #'parse-premise (rest lhs))
                               (mapcar #'parse-conclusion (rest rhs))
                               :docstring (and docstring-p (cst:raw docstring-cst))
                               :cst cst))))))))))

(defun parse-toplevel (cst)
  "Parse CST into one of a PRED-DECL, INIT-FACT, or RULE AST node,
signaling SYNTAX-ERROR if CST is malformed."
  (cond ((cst:null cst)
         (error 'syntax-error
                :cst cst
                :message "empty toplevel form; expected defpred, deffact, or defrule"))
        ((cst:atom cst)
         (error 'syntax-error
                :cst cst
                :message (format nil "toplevel form is not a list: ~s"
                                 (cst:raw cst))))
        ((not (cst:proper-list-p cst))
         (error 'syntax-error
                :cst cst
                :message "toplevel form is not a proper list"))
        (t (let* ((head-cst (cst:first cst))
                  (head (cst:raw head-cst)))
             (cond ((or (not (symbolp head))
                        (keywordp head))
                    (error 'syntax-error
                           :cst head-cst
                           :message (format nil "invalid toplevel form: ~s; expected defpred, deffact, or defrule"
                                            head)))
                   (t (alexandria:switch (head :test #'string=)
                        ('defpred (parse-pred-decl cst))
                        ('deffact (parse-init-fact cst))
                        ('defrule (parse-rule cst))
                        (t (error 'syntax-error
                                  :cst head-cst
                                  :message (format nil "unknown toplevel form: ~s; expected defpred, deffact, or defrule"
                                                   head))))))))))
