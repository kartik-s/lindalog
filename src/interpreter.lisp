;;;; interpreter.lisp

(in-package #:lindalog)

(defstruct (activation
            (:constructor make-activation (refract-p key duration)))
  (refract-p nil :type boolean
               :read-only t)
  (key nil :type list
           :read-only t)
  (duration nil :type symbol
                :read-only t))

(defun fire-rule (rule image)
  (let ((database (image-database image))
        (env (executable-env (image-executable image))))
    (labels ((refraction-key (match)
               (cons (rule-name rule)
                     (mapcar #'fact-id (match-read-facts match))))
             (signal-premise-p (premise)
               (eq :sig (pred-decl-store
                         (lookup-pred-decl
                          (atom-predicate (premise-atom premise))
                          env))))
             (refracted-p (match)
               (when (null (match-consumed-facts match))
                 (let ((key (refraction-key match)))
                   (or (gethash key (image-permanent-refractions image))
                       (gethash key (image-cycle-refractions image)))))))
      (let* ((match (match-premises (rule-lhs rule)
                                    database
                                    env
                                    (make-match)
                                    (complement #'refracted-p)))
             (activation (if (eq match +match-fail+)
                             nil
                             (let ((refract-p (null (match-consumed-facts match))))
                               (make-activation
                                refract-p
                                (refraction-key match)
                                (when refract-p
                                  (if (some #'signal-premise-p (rule-lhs rule))
                                      :cycle
                                      :permanent)))))))
        (when activation
          (when (activation-refract-p activation)
            (let ((table (ecase (activation-duration activation)
                           (:permanent (image-permanent-refractions image))
                           (:cycle (image-cycle-refractions image)))))
              (setf (gethash (activation-key activation) table)
                    t)))
          (dolist (fact (match-consumed-facts match))
            (remove-fact fact database))
          (dolist (conclusion (rule-rhs rule))
            (add-fact (subst-atom conclusion (match-bindings match))
                      database
                      env))
          (values activation))))))

(defun react (image atoms)
  (let* ((exe (image-executable image))
         (program (executable-program exe))
         (env (executable-env exe))
         (rules (program-rules program)))
    ;; Add facts
    (dolist (atom atoms)
      (add-fact atom (image-database image) env))
    ;; Fire rules until quiescence
    (loop :while (some (lambda (rule)
                         (fire-rule rule image))
                       rules))
    ;; Clear signals
    (dolist (pred-decl (program-pred-decls program))
      (when (eq :sig (pred-decl-store pred-decl))
        (setf (fill-pointer (gethash (pred-decl-name pred-decl)
                                     (database-facts-by-pred (image-database image))))
              0)))
    ;; Clear cycle refractions
    (clrhash (image-cycle-refractions image))))
