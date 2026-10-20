;;;; parser.lisp

(in-package #:lindalog)

(defstruct (clause-spec
            (:constructor clause-spec (keyword template &key required-p)))
  (keyword nil :type keyword :read-only t)
  (template nil :type string :read-only t)
  (required-p nil :type boolean :read-only t))

(defparameter *pred-decl-clauses*
  (list (clause-spec :store "(:store {:rd|:in|:sig})" :required-p t)
        (clause-spec :doc "(:doc <string>)")))

(defparameter *init-fact-clauses*
  (list (clause-spec :doc "(:doc <string>)")))

(defparameter *rule-clauses*
  (list (clause-spec :when "(:when <atom>+)" :required-p t)
        (clause-spec :then "(:then <atom>*)" :required-p t)
        (clause-spec :doc "(:doc <string>)")))

(defun variable-symbol-p (x)
  "Check if X is a non-keyword symbol whose name is a question mark
followed by at least one more character."
  (and (symbolp x)
       (not (keywordp x))
       (<= 2 (length (symbol-name x)))
       (equal (char (symbol-name x) 0)
              #\?)))

(defparameter *reserved-names* '(defrule defpred deffact rd in sig nil))
(defparameter *reserved-keywords* '(:rd :in :sig))

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
                (not (keywordp form))
                (not (reserved-name-p form)))
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

(defun collect-clauses (cst clauses-cst clause-specs)
  "Collect the clauses specified by CLAUSE-SPECS from CLAUSES-CST, a list
of CSTs for each clause. Raise a SYNTAX-ERROR pointing at CST for any
missing required clauses, or at the CST for an unknown clause."
  (let* ((required-specs
           (remove-if-not (lambda (spec)
                            (clause-spec-required-p spec))
                          clause-specs))
         (clauses
           (reduce (lambda (clauses clause-cst)
                     (let ((clause (cst:raw clause-cst)))
                       (if (or (null clause)
                               (not (alexandria:proper-list-p clause)))
                           (error 'syntax-error
                                  :cst clause-cst
                                  :message "clause must be a proper list headed by a keyword")
                           (let ((keyword (first clause)))
                             (cond ((not (find keyword clause-specs :key #'clause-spec-keyword))
                                    (error 'syntax-error
                                           :cst (cst:first clause-cst)
                                           :message (format nil "unknown clause ~s, expected one of: ~{~a~^, ~}"
                                                            keyword
                                                            (mapcar #'clause-spec-template clause-specs))))
                                   ((assoc keyword clauses)
                                    (error 'syntax-error
                                           :cst (cst:first clause-cst)
                                           :message (format nil "duplicate clause: ~s"
                                                            keyword)))
                                   (t (acons keyword clause-cst clauses)))))))
                   clauses-cst
                   :initial-value nil))
         (missing (remove-if (lambda (spec)
                               (assoc (clause-spec-keyword spec) clauses))
                             required-specs)))
    (if missing
        (error 'syntax-error
               :cst cst
               :message (format nil "missing required clause~p: ~{~a~^, ~}"
                                (length missing)
                                (mapcar #'clause-spec-template missing)))
        clauses)))

(defun parse-pred-decl (cst)
  "Parse CST into a PRED-DECL AST node, signaling SYNTAX-ERROR if CST
is malformed."
  (let ((parts (if (cst:proper-list-p cst)
                    (cst:listify cst)
                    (error 'syntax-error
                           :cst cst
                           :message "predicate is not a proper list"))))
    (if (> 3 (length parts))
        (error 'syntax-error
               :cst cst
               :message "predicate declaration must be of the form (defpred <name> (<arg-name>*) (:store {:rd|:in|:sig}) (:doc <string>)?")
        (let* ((name-cst (second parts))
               (name (cst:raw name-cst))
               (args-cst (third parts)))
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
                ((not (cst:proper-list-p args-cst))
                 (error 'syntax-error
                        :cst args-cst
                        :message "predicate arguments must be a proper list"))
                (t (let* ((clauses (collect-clauses cst (cdddr parts) *pred-decl-clauses*))
                          (store-cst (cdr (assoc :store clauses)))
                          (doc-cst (cdr (assoc :doc clauses))))
                     (cond ((not (= 2 (length (cst:raw store-cst))))
                            (error 'syntax-error
                                   :cst store-cst
                                   :message ":store clause must be of the form (:store {:rd|:in|:sig})"))
                           ((not (member (cst:raw (cst:second store-cst)) '(:rd :in :sig)))
                            (error 'syntax-error
                                   :cst (cst:second store-cst)
                                   :message (format nil "expected :rd, :in, or :sig for predicate store, got: ~s"
                                                    (cst:raw (cst:second store-cst)))))
                           ((and doc-cst
                                 (or (not (= 2 (length (cst:raw doc-cst))))
                                     (not (stringp (cst:raw (cst:second doc-cst))))))
                            (error 'syntax-error
                                   :cst doc-cst
                                   :message ":doc clause must be of the form (:doc <string>)"))
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
                                    :finally (return (make-pred-decl name (nreverse parsed-args)
                                                                     (cst:raw (cst:second store-cst))
                                                                     :docstring (when doc-cst (cst:raw (cst:second doc-cst)))
                                                                     :cst cst))))))))))))

(defun parse-init-fact (cst)
  "Parse CST into an INIT-FACT AST node, signaling SYNTAX-ERROR if CST
is malformed."
  (let ((parts (if (cst:proper-list-p cst)
                   (cst:listify cst)
                   (error 'syntax-error
                          :cst cst
                          :message "fact is not a proper list"))))
    (if (> 2 (length parts))
        (error 'syntax-error
               :cst cst
               :message "fact definition must be of the form (deffact <atom> (:doc <string>)?)")
        (let* ((atom (parse-atom (second parts)))
               (vars (atom-variables atom))
               (clauses (collect-clauses cst (cddr parts) *init-fact-clauses*))
               (doc-cst (cdr (assoc :doc clauses))))
          (if (and doc-cst
                   (or (not (= 2 (length (cst:raw doc-cst))))
                       (not (stringp (cst:raw (cst:second doc-cst))))))
              (error 'syntax-error
                     :cst doc-cst
                     :message ":doc clause must be of the form (:doc <string>)")
              (if (null vars)
                  (make-init-fact atom :docstring (when doc-cst (cst:raw (cst:second doc-cst)))
                                       :cst cst)
                  (error 'syntax-error
                         :cst (ast-node-cst (first vars))
                         :message (format nil "fact atom contains a variable and is therefore not ground: ~s"
                                          (variable-name (first vars))))))))))

(defun parse-rule (cst)
  "Parse CST into a RULE AST node, signaling SYNTAX-ERROR if CST is
malformed."
  (let ((parts (if (cst:proper-list-p cst)
                    (cst:listify cst)
                    (error 'syntax-error
                           :cst cst
                           :message "rule is not a proper list"))))
    (if (> 2 (length parts))
        (error 'syntax-error
               :cst cst
               :message "rule definition must be of the form (defrule <name> (:when <premise>+) (:when <atom>*) (:doc <string>)?")
        (let* ((name-cst (second parts))
               (name (cst:raw name-cst)))
          (cond ((null name)
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
                (t (let* ((clause-csts (collect-clauses cst (cddr parts) *rule-clauses*))
                          (doc-cst (cdr (assoc :doc clause-csts)))
                          (when-cst (cdr (assoc :when clause-csts)))
                          (then-cst (cdr (assoc :then clause-csts))))
                     (cond ((and doc-cst
                                 (or (not (cst:rest doc-cst))
                                     (not (= 2 (length (cst:raw doc-cst))))
                                     (not (stringp (cst:raw (cst:second doc-cst))))))
                            (error 'syntax-error
                                   :cst doc-cst
                                   :message ":doc clause must be of the form (:doc <string>)"))
                           ((not when-cst)
                            (error 'syntax-error
                                   :cst when-cst
                                   :message ":when clause must be of the form (:when <atom>+)"))
                           ((cst:null (cst:rest when-cst))
                            (error 'syntax-error
                                   :cst when-cst
                                   :message ":when clause has no premises"))
                           ((not then-cst)
                            (error 'syntax-error
                                   :cst then-cst
                                   :message ":then clause must be of the form (:then <atom>*)"))
                           (t (make-rule
                               name
                               (mapcar #'parse-premise (rest (cst:listify when-cst)))
                               (mapcar #'parse-conclusion (rest (cst:listify then-cst)))
                               :docstring (and doc-cst (cst:raw (cst:second doc-cst)))
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

(defun parse-program (csts)
  "Parse a program from a list of toplevel CSTs into a PROGRAM struct,
signaling PROGRAM-SYNTAX-ERRORS if program is malformed."
  (loop :for cst :in csts
        :for form := (handler-case
                         (parse-toplevel cst)
                       (syntax-error (e) e))
        :if (pred-decl-p form)
          :collect form :into predicates
          :else :if (init-fact-p form)
                  :collect form :into facts
                  :else :if (rule-p form)
                          :collect form :into rules
                          :else :if (typep form 'syntax-error)
                                  :collect form :into errors
        :finally (if errors
                     (error 'program-syntax-errors
                            :errors errors)
                     (return (make-program predicates facts rules)))))
