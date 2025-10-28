;; This file is part of the Implementation of
;; Method Combination Types as proposed by Didier Verna
;; in HAL Id: hal-04751233 https://hal.science/hal-04751233v1
;; adapted for ABCL by Julius Borghardt https://github.com/juliusborghardt

(in-package :method-combination-types)


(defconstant **method-combination-types** (make-hash-table :test 'eq)
  "The global method combination types hash table.
This hash table maps names to method combination types.")

;; #### NOTE: not SETF method here. We wouldn't want it to be public.
;; -- didier
(defun find-method-combination-type (name &optional (errorp t))
  "Find a NAMEd method combination type.
If ERRORP (the default), throw an error if no such method combination type is
found. Otherwise, return NIL."
  (or (gethash name **method-combination-types**)
      (when errorp
        (error "There is no method combination type named ~A." name))))

(defmethod find-method-combination
    ((generic-function generic-function) name options)
  "Find a method combination object for type NAME and options.
If no method combination type exists by that NAME, return NIL.
Otherwise, a (potentially new) method combination object is returned.
The GENERIC-FUNCTION argument is ignored."
  (let ((type (find-method-combination-type name nil)))
    (when type
      (or (gethash options (method-combination-type-%cache type))
          (setf (gethash options (method-combination-type-%cache type))
                (funcall (method-combination-%constructor type)
                  options))))))

(defun load-defcombin
    (name new documentation &aux (old (find-method-combination-type name nil)))
  "Register NEW method combination type under NAME with DOCUMENTATION.
This function takes care of any potential redefinition of an existing method
combination type."
  (when old
    (setf (slot-value new '%cache) (method-combination-type-%cache old))
    (maphash (lambda (options combination)
               (declare (ignore options))
               (change-class combination new))
             (method-combination-type-%cache new)))
  (setf (gethash name **method-combination-types**) new)
  (setf (random-documentation name 'method-combination) documentation)
  name)



;; ===================
;; Method Combinations
;; ===================

;; This section completes the minimal instalment of the method combinations
;; hierarchy that was elaborated in defs.lisp.

;; from sbcl/src/pcl/dfun.lisp
(defun flush-effective-method-cache (generic-function)
  (dolist (method (generic-function-methods generic-function))
    (let ((cache
           (if (listp method) (sixth method) (method-em-cache method))))
      (when cache
        (rplaca cache nil)
        (rplacd cache nil)))))


(defmethod update-generic-function-for-redefined-method-combination
    ((function generic-function)
     (previous standard-method-combination)
     (current standard-method-combination))
  "Flush the effective method cache and reinitialize FUNCTION."
  (flush-effective-method-cache function)
  (reinitialize-instance function))



;; oversimplified reimplementation to replace hashset.lisp from sbcl -- JB
;; no concurrency!
(defun map-hashset (function hashset)
  "Apply FUNCTION to every element of HASHSET (a hash-table or similar)."
  (maphash (lambda (key value)
             (declare (ignore value))
             (funcall function key))
           hashset))


(defmethod update-instance-for-different-class :after
    ((previous standard-method-combination)
     (current standard-method-combination)
     &key &allow-other-keys)
  "Inform every function using CURRENT method combination that it has changed."
  (map-hashset
   (lambda (gf)
     (update-generic-function-for-redefined-method-combination
      gf previous current))
   ;;(method-combination-%generic-functions current)))
   (long-method-combination-generic-function-symbol current)))


;; -----------------------------------
;; Method combination pseudo-accessors
;; -----------------------------------

;; These provide direct access to properties that belong to the method
;; combination type rather to the method combination itself.

;; #### TODO: some potentially usefull information is missing (like the
;; generic-function-symbol of the long form), or, maybe some of those are
;; actually useless. I need to verify which stuff is actually used (like, in
;; compute-effective-method etc.).
;; -- didier

(defmethod method-combination-type-name
    ((combination standard-method-combination))
  "Return method COMBINATION's type name."
  (method-combination-type-name (class-of combination)))

(defmethod method-combination-lambda-list
    ((combination standard-method-combination))
  "Return method COMBINATION's lambda-list."
  (method-combination-type-lambda-list (class-of combination)))

(defmethod short-method-combination-operator
    ((combination short-method-combination))
  "Return short method COMBINATION's operator."
  (short-method-combination-type-operator (class-of combination)))

(defmethod short-method-combination-identity-with-one-argument
    ((combination short-method-combination))
  "Return short method COMBINATION's identity-with-one-argument."
  (short-method-combination-type-identity-with-one-argument
   (class-of combination)))

(defmethod long-method-combination-%args-lambda-list
    ((combination long-method-combination))
  "Return long method COMBINATION's args-lambda-list."
  (long-method-combination-type-%args-lambda-list (class-of combination)))


;; ---------------------------
;; standard method combination
;; ---------------------------

;; #### WARNING: we work with CLOS layer 1 (the macro level) below because
;; it's much simpler to create the specialization of COMPUTE-PRIMARY-METHODS
;; this way. The unfortunate side effect is that the class below is defined
;; globally, which I don't really want (all other concrete method combination
;; classes are anonymous; even the built-in short ones).
;; -- didier

(defmethod compute-primary-methods
    ((gf generic-function)
     (combin standard-standard-method-combination)
     applicable-methods)
  (remove-if #'method-qualifiers applicable-methods))



;; -------------------------
;; short method combinations
;; -------------------------

;; Short method combinations all follow the same rule for computing the
;; effective method. So, we just implement that rule once. Each short method
;; combination object just reads the parameters out of the object and runs the
;; same rule.

(defmethod initialize-instance :before
    ((instance short-method-combination)
     &key options &allow-other-keys
     &aux (name (method-combination-type-name instance)))
  "Check the validity of OPTIONS for a short method combination INSTANCE."
  (when (cdr options)
    (method-combination-error
     "Illegal options to the ~S short method combination.~%~
      Short method combinations accept a single ORDER argument."
     name))
  (unless (member (car options) '(:most-specific-first :most-specific-last))
    (method-combination-error
     "Illegal ORDER option to the ~S short method combination.~%~
      ORDER must be either :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST."
     name)))

(defun load-short-defcombin
    (name operator identity-with-one-argument documentation
     mc-class mct-spec
     source-location
     &aux (mc-class (find-class mc-class))
          (mct-class (find-class (if (symbolp mct-spec)
                                   mct-spec
                                   (car mct-specx)))))
  "Register a new short method combination type under NAME."
  (unless (subtypep mc-class 'short-method-combination)
    (method-combination-error
     "Invalid method combination class: ~A.~%~
      When defining a method combination type in short form, the provided~%~
      method combination class must be a subclass of SHORT-METHOD-COMBINATION."
     mc-class))
  (unless (subtypep mct-class 'short-method-combination-type)
    (method-combination-error
     "Invalid method combination type class: ~A.~%~
      When defining a method combination type in short form, the provided~%~
      method combination type class must be a subclass of
      SHORT-METHOD-COMBINATION-TYPE."
     mct-class))
  ;; #### NOTE: we can't change-class class metaobjects, so we need to
  ;; recreate a brand new one.
  (let ((new (apply #'make-instance mct-class
                    'source source-location
                    :direct-superclasses (list mc-class)
                    :documentation documentation
                    :type-name name
                    :operator operator
                    :identity-with-one-argument identity-with-one-argument
                    (when (consp mct-spec) (cdr mct-spec)))))
    (setf (slot-value new '%constructor)
          (lambda (options)
            (funcall #'make-instance
              ;; #### NOTE: in principle, short method combinations would only
              ;; have at most two different instances, because the only
              ;; possible choice for options is :MOST-SPECIFIC-FIRST or
              ;; :MOST-SPECIFIC-LAST. However, thanks to the line below, the
              ;; caches will keep track of the options provided by the
              ;; programmer. It's nice because it's informative. As a result,
              ;; if the method combination is used without any argument, or
              ;; explicitly with :MOST-SPECIFIC-FIRST, we will end up with 2
              ;; different yet identical instances (so possibly 3 in total if
              ;; :MOST-SPECIFIC-LAST appears as well). Not such a big deal.
              new :options (or options '(:most-specific-first)))))
    (load-defcombin name new documentation)))


;; from sbcl/src/pcl/combin.lisp
(defun short-method-combination-qualifiers (type-name)
  (list type-name :around))

(defun short-method-combination-qualifier-p (type-name qualifier)
  (or (eq qualifier type-name) (eq qualifier :around)))


(defmethod invalid-qualifiers
    ((gf generic-function) (combin short-method-combination) method)
  (let* ((qualifiers (method-qualifiers method))
         (qualifier (first qualifiers))
         (type-name (method-combination-type-name combin))
         (why (cond
                ((null qualifiers)
                 "has no qualifiers")
                ((cdr qualifiers)
                 "has too many qualifiers")
                (t
                 (aver (not (short-method-combination-qualifier-p
                             type-name qualifier)))
                 "has an invalid qualifier"))))
    (invalid-method-error
     method
     "~@<The method ~S on ~S ~A.~
      ~@:_~@:_~
      The method combination type ~S was defined with the short form ~
      of DEFINE-METHOD-COMBINATION and so requires all methods have ~
      either ~{the single qualifier ~S~^ or ~}.~@:>"
     method gf why type-name (short-method-combination-qualifiers type-name))))

(defmethod compute-primary-methods ((gf generic-function)
                                    (combin short-method-combination)
                                    applicable-methods)
  (let ((type-name (method-combination-type-name combin)))
    (remove-if-not (lambda (m) (let ((qs (method-qualifiers m)))
                                 (and (eql (car qs) type-name)
                                      (null (cdr qs)))))
                   applicable-methods)))



;; ------------------------
;; Long method combinations
;; ------------------------

(define-condition simple-program-error (simple-condition program-error) ())

;(define-error-wrapper %program-error (&optional datum &rest arguments)
;  (error (apply #'coerce-to-condition datum
;                'simple-program-error '%program-error arguments)))

(defun expand-long-defcombin (form)
  (let ((type-name (cadr form))
        (lambda-list (caddr form))
        (method-group-specifiers-presentp (cdddr form))
        (method-group-specifiers (cadddr form))
        (body (cddddr form))
        (args-option ())
        (gf-var nil)
        (mc-class 'long-method-combination)
        (mct-spec '(long-method-combination-type)))
    (unless method-group-specifiers-presentp
      (error
       "~@<The long form of ~S requires a list of method group specifiers.~:@>"
       'define-method-combination))
    (when (and (consp (car body)) (eq (caar body) :arguments))
      (setq args-option (cdr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :generic-function))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (error
         "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
         :generic-function 'define-method-combination))
      (setq gf-var (cadr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :method-combination-class))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (error
         "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
         :method-combination-class 'define-method-combination))
      (setq mc-class (cadr (pop body))))
    (when (and (consp (car body))
               (eq (caar body) :method-combination-type-class))
      (setq mct-spec (cdr (pop body))))
    (multiple-value-bind (documentation function)
        (make-long-method-combination-function
         type-name lambda-list method-group-specifiers args-option gf-var
         body)
      `(load-long-defcombin
        ',type-name ',documentation #',function ',lambda-list
        ;;',args-option ',mc-class ',mct-spec (sb-c:source-location)))))
	',args-option ',mc-class ',mct-spec (or *compile-file-pathname* *load-pathname*)))))

(defun load-long-defcombin
    (name documentation function lambda-list args-lambda-list
     mc-class mct-spec
     source-location
     &aux (mc-class (find-class mc-class))
          (mct-class (find-class (car mct-spec))))
  (unless (subtypep mc-class 'long-method-combination)
    (method-combination-error
     "Invalid method combination class: ~A.~%~
      When defining a method combination type in long form, the provided~%~
      method combination class must be a subclass of LONG-METHOD-COMBINATION."
     mc-class))
  (unless (subtypep mct-class 'long-method-combination-type)
    (method-combination-error
     "Invalid method combination type class: ~A.~%~
      When defining a method combination type in long form, the provided~%~
      method combination type class must be a subclass of
      LONG-METHOD-COMBINATION-TYPE."
     mct-class))
  ;; #### NOTE: we can't change-class class metaobjects, so we need to
  ;; recreate a brand new one.
  ;; -- didier
  (let ((new (apply #'make-instance mct-class
                    'source source-location
                    :direct-superclasses (list mc-class)
                    :documentation documentation
                    :type-name name
                    :lambda-list lambda-list
                    :args-lambda-list args-lambda-list
                    :function function
                    (cdr mct-spec))))
    (setf (slot-value new '%constructor)
          (lambda (options) (funcall #'make-instance new :options options)))
    (load-defcombin name new documentation)))

(defmethod compute-effective-method
    ((function generic-function)
     (combination long-method-combination)
     applicable-methods)
  "Call the long method COMBINATION type's specific function."
  (funcall (long-method-combination-type-%function (class-of combination))
    function combination applicable-methods))

(define-condition simple-style-warning (simple-condition style-warning) ())
(defun style-warn (datum &rest arguments)
  ;; Cross-compiler needs a special-case for DATUM being a string,
  ;; because it needs to produce a SIMPLE-STYLE-WARNING, not SIMPLE-WARNING.
  ;; The SBCL-specific %WARN function - which allows specifying the default
  ;; condition class when handed a string - exists only on the target lisp.
  (if (stringp datum)
      (warn 'simple-style-warning
            :format-control datum :format-arguments arguments)
      (apply #'warn datum arguments)))

(defun parse-body (body doc-string-allowed &optional silent)
  (flet ((doc-string-p (x remaining-forms doc)
           (and (stringp x) doc-string-allowed
                  ;; ANSI 3.4.11 explicitly requires that a doc string
                  ;; be followed by another form (either an ordinary form
                  ;; or a declaration). Hence:
                remaining-forms
                ))
         (declaration-p (x)
           (when (listp x)
             (let ((name (car x)))
               (cond ((eq name 'declare) t)
                     (t
                      (when (and (eq name 'declaim) (not silent))
                        ;; technically legal, but rather unlikely to
                        ;; be what the user meant to do...
                        (style-warn
                         "DECLAIM where DECLARE was probably intended"))
                      nil))))))
    (let ((forms body) (decls (list nil)) (doc nil))
      (declare (dynamic-extent decls))
      (let ((decls decls))
        (loop (when (endp forms) (return))
              (let ((form (first forms)))
                (cond ((doc-string-p form (rest forms) doc)
                       (setq doc form))
                      ((declaration-p form)
                       (setq decls (setf (cdr decls) (list form))))
                      (t
                       (return))))
              (setq forms (rest forms))))
      (values forms (cdr decls) doc))))


(defun make-long-method-combination-function
       (type-name ll method-group-specifiers args-option gf-var body)
  (declare (ignore type-name))
  (multiple-value-bind (real-body declarations documentation)
      (parse-body body t)
    (let ((wrapped-body
            (wrap-method-group-specifier-bindings method-group-specifiers
                                                  declarations
                                                  real-body)))
      (when gf-var
        (push `(,gf-var .generic-function.) (cadr wrapped-body)))

      (when args-option
        (setq wrapped-body (deal-with-args-option wrapped-body args-option)))

      (when ll
        (setq wrapped-body
              `(apply #'(lambda ,ll ,wrapped-body)
                      (method-combination-options .method-combination.))))

      (values
        documentation
        `(lambda (.generic-function. .method-combination. .applicable-methods.)
           (declare (ignorable .generic-function.
                     .method-combination. .applicable-methods.))
           (block .long-method-combination-function. ,wrapped-body))))))

(define-condition long-method-combination-error
    (reference-condition simple-error)
  ()
  (:default-initargs
   :references '((:ansi-cl :macro define-method-combination))))

;;; NOTE:
;;;
;;; The semantics of long form method combination in the presence of
;;; multiple methods with the same specializers in the same method
;;; group are unclear by the spec: a portion of the standard implies
;;; that an error should be signalled, and another is more lenient.
(defun group-cond-clause (name tests specializer-cache order-matters-test)
  (let ((maybe-error-clause
          `(if (and ,order-matters-test
                    (equal ,specializer-cache .specializers.)
                    (not (null .specializers.)))
               (return-from .long-method-combination-function.
                 '(error 'long-method-combination-error
                   :format-control "More than one method of type ~S ~
                                       with the same specializers."
                   :format-arguments (list ',name)))
               (setq ,specializer-cache .specializers.))))
    `((or ,@tests)
      ,maybe-error-clause
      (push .method. ,name))))


;; From sbcl/src/compiler/early-contantp.lisp
(declaim (inline constantp))
(defun constantp (form &optional (environment nil envp))
  "True of any FORM that has a constant value: self-evaluating objects,
keywords, defined constants, quote forms. Additionally the
constant-foldability of some function calls and special forms is recognized.
If ENVIRONMENT is provided, the FORM is first macroexpanded in it."
  (%constantp form environment envp))

(defun %constantp (form environment envp)
  ;; Pick off quasiquote prior to macroexpansion.
  (when (typep form '(cons (eql quasiquote) (cons t null)))
    (return-from %constantp
      (constant-quasiquote-form-p (cadr form) environment envp)))
  (let ((form (if envp
                  (handler-case
                      (%macroexpand form environment)
                    (error ()
                      (return-from %constantp)))
                  form)))
    (typecase form
      ;; This INFO test catches KEYWORDs as well as explicitly
      ;; DEFCONSTANT symbols.
      (symbol
       (or (eq (info :variable :kind form) :constant)
           (constant-special-variable-p form)))
      (list
       (let ((answer (constant-special-form-p form environment envp)))
         (if (eq answer :maybe)
             (values (constant-function-call-p form environment envp))
             answer)))
      (t t))))

(defun constant-quasiquote-form-p (expr environment envp)
  ;; This is an utter cinch because we haven't macroexpanded.
  ;; Parse just enough to recognize (DEFTYPE <T2> () (<T1> ,THING)) etc.
  (named-let recurse ((expr expr))
    (cond ((atom expr)
           (cond ((comma-p expr)
                  (%constantp (comma-expr expr) environment envp))
                 ((simple-vector-p expr) (every #'recurse expr))
                 (t)))
          ((eq (car expr) 'quasiquote) nil) ; give up
          (t (and (recurse (car expr)) (recurse (cdr expr)))))))

(defun %constant-form-value (form environment envp)
  (let ((form (if (or envp
                      (typep form '(cons (eql quasiquote) (cons t null))))
                  (%macroexpand form environment)
                  form)))
    (typecase form
      (symbol
       (symbol-value form))
      (list
       (multiple-value-bind (specialp value)
           (constant-special-form-value form environment envp)
         (if specialp value (constant-function-call-value
                             form environment envp))))
      (t
       form))))

(defun constant-special-variable-p (name)
  (and (member name *special-constant-variables*) t))

(declaim (inline constant-form-value))
(defun constant-form-value (form &optional (environment nil envp))
  "Returns the value of the constant FORM in ENVIRONMENT. Behaviour
is undefined unless CONSTANTP has been first used to determine the
constantness of the FORM in ENVIRONMENT."
  (%constant-form-value form environment envp))


(defun constant-function-call-value (form environment envp)
  (apply (fdefinition (car form))
         (mapcar (lambda (arg)
                   (%constant-form-value arg environment envp))
                 (cdr form))))


(defun %macroexpand (form &optional env)
  (labels ((frob (form expanded)
             (multiple-value-bind (new-form newly-expanded-p)
                 (%macroexpand-1 form env)
               (if newly-expanded-p
                   (frob new-form t)
                   (values new-form expanded)))))
    (frob form nil)))

(defun %constant-form-value (form environment envp)
  (let ((form (if (or envp
                      (typep form '(cons (eql quasiquote) (cons t null))))
                  (%macroexpand form environment)
                  form)))
    (typecase form
      (symbol
       (symbol-value form))
      (list
       (multiple-value-bind (specialp value)
           (constant-special-form-value form environment envp)
         (if specialp value (constant-function-call-value
                             form environment envp))))
      (t
       form))))


;; all from sbcl
(defun constant-special-form-p (form environment envp)
    (let (result)
      (tagbody (setq result (expand-cases 1 :maybe)) fail)
      result))

(defun constant-special-form-value (form environment envp)
    (let ((result))
      (tagbody
         (setq result (expand-cases 2 (return-from constant-special-form-value
                                        (values nil nil))))
         (return-from constant-special-form-value (values t result))
       fail))
    ;; Mutatation of FORM could cause failure. It's user error, not a bug.
    (error "CONSTANT-FORM-VALUE called with invalid expression ~S" form))



(defun wrap-method-group-specifier-bindings
    (method-group-specifiers declarations real-body)
  (let (names specializer-caches cond-clauses required-checks order-vars order-cleanups)
    (let ((nspecifiers (length method-group-specifiers)))
      (dolist (method-group-specifier method-group-specifiers
               (push `(t (return-from .long-method-combination-function.
                           `(invalid-method-error , .method.
                             "~@<is applicable, but does not belong ~
                              to any method group~@:>")))
                     cond-clauses))
        (multiple-value-bind (name tests description order required)
            (parse-method-group-specifier method-group-specifier)
          (declare (ignore description))
          (let* ((specializer-cache (gensym))
                 (order-var (gensym "O"))
                 (order-constantp (constantp order))
                 (order-value (and order-constantp (constant-form-value order))))
            (push name names)
            (push specializer-cache specializer-caches)
            (unless order-constantp
              (push `(,order-var ,order) order-vars))
            (let ((order-matters-test
                    (cond
                      ;; It is reasonable to allow a single method
                      ;; group of * to bypass all rules, as this is
                      ;; explicitly stated in the standard.
                      ((and (eq (cadr method-group-specifier) '*)
                            (= nspecifiers 1))
                       nil)
                      ;; an :ORDER value known at compile-time to be
                      ;; NIL (an SBCL extension) also bypasses the
                      ;; ordering checks.  (Other :ORDER values do
                      ;; not.)
                      (order-constantp (not (eql order-value nil)))
                      ;; otherwise, check the ORDER value at
                      ;; method-combination time, bypassing ordering
                      ;; checks if it is NIL.
                      (t `(not (eql ,order-var nil))))))
              (push (group-cond-clause name tests specializer-cache order-matters-test)
                    cond-clauses))
            (when required
              (push `(when (null ,name)
                      (return-from .long-method-combination-function.
                        '(error 'long-method-combination-error
                          :format-control "No ~S methods."
                          :format-arguments (list ',name))))
                    required-checks))
            (cond
              ((and order-constantp (eq order-value :most-specific-first))
               (push `(setq ,name (nreverse ,name)) order-cleanups))
              ((and order-constantp
                    (or (null order-value) (eq order-value :most-specific-last))))
              (t (push `(ecase ,order-var
                          (:most-specific-first (setq ,name (nreverse ,name)))
                          ((nil :most-specific-last)))
                       order-cleanups))))))
      `(let (,@(nreverse names) ,@specializer-caches ,@order-vars)
        (declare (ignorable ,@specializer-caches))
        ,@declarations
        (dolist (.method. .applicable-methods.)
          (let ((.qualifiers. (method-qualifiers .method.))
                (.specializers. (method-specializers .method.)))
            (declare (ignorable .qualifiers. .specializers.))
            (cond ,@(nreverse cond-clauses))))
        ,@(nreverse required-checks)
        ,@(nreverse order-cleanups)
         ,@real-body))))

(defun memq (e l)
  (do ((current l (cdr current)))
      ((atom current) nil)
    (when (eq (car current) e) (return current))))

(defun parse-method-group-specifier (method-group-specifier)
  (unless (symbolp (car method-group-specifier))
    (error "~@<Method group specifiers in the long form of ~S ~
                     must begin with a symbol.~:@>" 'define-method-combination))
  (let* ((name (pop method-group-specifier))
         (patterns ())
         (tests
           (let (collect)
             (block collect-tests
               (loop
                 (if (or (null method-group-specifier)
                         (memq (car method-group-specifier)
                               '(:description :order :required)))
                     (return-from collect-tests t)
                     (let ((pattern (pop method-group-specifier)))
                       (push pattern patterns)
                       (push (parse-qualifier-pattern name pattern)
                             collect)))))
             (nreverse collect))))
    (when (null patterns)
      (error "~@<Method group specifiers in the long form of ~S ~
                       must have at least one qualifier pattern or predicate.~@:>"
                      'define-method-combination))
    (values name
            tests
            (getf method-group-specifier :description
                  (make-default-method-group-description patterns))
            (getf method-group-specifier :order :most-specific-first)
            (getf method-group-specifier :required nil))))

(defun parse-qualifier-pattern (name pattern)
  (cond ((eq pattern '()) `(null .qualifiers.))
        ((eq pattern '*) t)
        ((symbolp pattern) `(,pattern .qualifiers.))
        ((listp pattern) `(qualifier-check-runtime ',pattern .qualifiers.))
        (t (error "In the method group specifier ~S,~%~
                   ~S isn't a valid qualifier pattern."
                  name pattern))))

(defun qualifier-check-runtime (pattern qualifiers)
  (loop (cond ((and (null pattern) (null qualifiers))
               (return t))
              ((eq pattern '*) (return t))
              ((and pattern qualifiers
                    (or (eq (car pattern) '*)
                        (eq (car pattern) (car qualifiers))))
               (pop pattern)
               (pop qualifiers))
              (t (return nil)))))

(defun make-default-method-group-description (patterns)
  (if (cdr patterns)
      (format nil
              "methods matching one of the patterns: ~{~S, ~} ~S"
              (butlast patterns) (car (last patterns)))
      (format nil
              "methods matching the pattern: ~S"
              (car patterns))))

;;; This baby is a complete mess. I can't believe we put it in this
;;; way. No doubt this is a large part of what drives MLY crazy.
;;;
;;; At runtime (when the effective-method is run), we bind an intercept
;;; lambda-list to the arguments to the generic function.
;;;
;;; At compute-effective-method time, the symbols in the :arguments
;;; option are bound to the symbols in the intercept lambda list.
(defun deal-with-args-option (wrapped-body args-lambda-list)
  (binding* (((llks required optional rest key aux env whole)
              (parse-lambda-list
               args-lambda-list
               :context "a define-method-combination arguments lambda list"
               :accept (lambda-list-keyword-mask '(&allow-other-keys &aux &key &optional &rest &whole)))))
    (check-lambda-list-names llks required optional rest key aux env whole
                             :context "a define-method-combination arguments lambda list"
                             :signal-via #'error)
    (let (intercept-rebindings)
      (flet ((intercept (sym) (push `(,sym ',sym) intercept-rebindings)))
        (when whole (intercept (car whole)))
        (dolist (arg required)
          (intercept arg))
        (dolist (arg optional)
          (multiple-value-bind (name default suppliedp)
              (parse-optional-arg-spec arg)
            (declare (ignore default))
            (intercept name)
            (when suppliedp (intercept (car suppliedp)))))
        (when rest (intercept (car rest)))
        (dolist (arg key)
          (multiple-value-bind (keyword name default suppliedp)
              (parse-key-arg-spec arg)
            (declare (ignore keyword default))
            (intercept name)
            (when suppliedp (intercept (car suppliedp)))))
        (dolist (arg aux)
          (intercept (if (consp arg) (car arg) arg)))
        ;; cosmetic only
        (setq intercept-rebindings (nreverse intercept-rebindings)))
      ;; This assumes that the head of WRAPPED-BODY is a let, and it
      ;; injects let-bindings of the form (ARG 'SYM) for all variables
      ;; of the argument-lambda-list; SYM is a gensym.
      (aver (memq (first wrapped-body) '(let let*)))
      (setf (second wrapped-body)
            (append intercept-rebindings (second wrapped-body)))
      ;; Be sure to fill out the args lambda list so that it can be too
      ;; short if it wants to.
      (unless (or (memq '&rest args-lambda-list)
                  (memq '&allow-other-keys args-lambda-list))
        (let ((aux (memq '&aux args-lambda-list)))
          (setq args-lambda-list
                (append (ldiff args-lambda-list aux)
                        (if (memq '&key args-lambda-list)
                            '(&allow-other-keys)
                            '(&rest .ignore.))
                        aux))))
      ;; .GENERIC-FUNCTION. is bound to the generic function in the
      ;; method combination function, and .GF-ARGS* is bound to the
      ;; generic function arguments in effective method functions
      ;; created for generic functions having a method combination that
      ;; uses :ARGUMENTS.
      ;;
      ;; The DESTRUCTURING-BIND binds the parameters of the
      ;; ARGS-LAMBDA-LIST to actual generic function arguments.  Because
      ;; ARGS-LAMBDA-LIST may be shorter or longer than the generic
      ;; function's lambda list, which is only known at run time, this
      ;; destructuring has to be done on a slighly modified list of
      ;; actual arguments, from which values might be stripped or added.
      ;;
      ;; Using one of the variable names in the body inserts a symbol
      ;; into the effective method, and running the effective method
      ;; produces the value of actual argument that is bound to the
      ;; symbol.
      `(let ((inner-result. ,wrapped-body)
             (gf-lambda-list (generic-function-lambda-list .generic-function.)))
         `(destructuring-bind ,',args-lambda-list
              ;; FIXME: we know enough (generic function lambda list,
              ;; args lambda list) at generate-effective-method-time
              ;; that we could partially evaluate this frobber, to
              ;; inline specific argument list manipulation rather
              ;; than the generic code currently contained in
              ;; FROB-COMBINED-METHOD-ARGS.
              (frob-combined-method-args
               .gf-args. ',gf-lambda-list
               ,',(length required) ,',(length optional))
            ,,(when (memq '.ignore. args-lambda-list)
                ''(declare (ignore .ignore.)))
            ;; If there is a &WHOLE in the args-lambda-list, let
            ;; it result in the actual arguments of the generic-function
            ;; not the frobbed list.
            ,,(when whole
                ``(setq ,',(car whole) .gf-args.))
            ,inner-result.)))))

;;; Partition VALUES into three sections: required, optional, and the
;;; rest, according to required, optional, and other parameters in
;;; LAMBDA-LIST.  Make the required and optional sections NREQ and
;;; NOPT elements long by discarding values or adding NILs, except
;;; don't extend the optional section when there are no more VALUES.
;;; Value is the concatenated list of required and optional sections,
;;; and what is left as rest from VALUES.
(defun frob-combined-method-args (values lambda-list nreq nopt)
  (loop with section = 'required
        for arg in lambda-list
        if (memq arg lambda-list-keywords) do
          (setq section arg)
          (unless (eq section '&optional)
            (loop-finish))
        else if (eq section 'required)
          count t into nr
          and collect (pop values) into required
        else if (and values (eq section '&optional))
          count t into no
          and collect (pop values) into optional
        finally
          (flet ((frob (list n m lengthenp)
                   (cond ((> n m) (butlast list (- n m)))
                         ((and (< n m) lengthenp) (nconc list (make-list (- m n))))
                         (t list))))
            (return (nconc (frob required nr nreq t)
                           (frob optional no nopt values)
                           values)))))




;; ===========
;; Entry Point
;; ===========

;; FIXME: according to ANSI 3.4.10 this is supposed to allow &WHOLE in the
;; long syntax. But it clearly does not, because if you write (&WHOLE v) then
;; you get (LAMBDA (&WHOLE V ...) ...) which is illegal

;; #### NOTE: according to Section 1.6 Language Extensions of the standard,
;; extending DEFINE-METHOD-COMBINATION with the :method-combination-class and
;; :method-combination-type-class options is conformant (it does not alter the
;; behavior of conforming code, and it is not explicitly prohibited).
;; -- didier

;; stolen frfom SBCL/src/compiler/proclaim.lisp
(defun check-designator (name macro &optional (predicate #'symbolp)
                                              (what "symbol")
                                              (arg-reference "NAME"))
  (unless (funcall predicate name)
    (error (format nil "The ~A argument to ~A, ~~S, is not a ~A."
                   arg-reference macro what)
           name)))


;; from sbcl/src/code/cross-misc.lisp
(defvar *unbound-marker* (make-symbol "UNBOUND-MARKER"))

(defun make-unbound-marker ()
  *unbound-marker*)

(defun unbound-marker-p (x)
  (eq x *unbound-marker*))

(defmacro define-method-combination (&whole form name . args)
  (declare (ignore args))
  (check-designator name 'define-method-combination)
  `(progn
     (with-single-package-locked-error
         (:symbol ',name "defining ~A as a method combination"))
     ,(if (and (cddr form)
               (listp (caddr form)))
        (expand-long-defcombin form)
        (let* ((type-name (cadr form))
               (doc (getf (cddr form) :documentation (make-unbound-marker)))
               (ioa (getf (cddr form) :identity-with-one-argument nil))
               (operator (getf (cddr form) :operator type-name))
               (mc-class (getf (cddr form) :method-combination-class
                               'short-method-combination))
               (mct-class (getf (cddr form) :method-combination-type-class
                                'short-method-combination-type)))
          (unless (or (unbound-marker-p doc) (stringp doc))
            (error
             "~@<~S argument to the short form of ~S must be a string.~:@>"
             :documentation 'define-method-combination))
          `(load-short-defcombin ',type-name ',operator ',ioa
                                 ,(unless (unbound-marker-p doc) doc)
                                 ',mc-class ',mct-class
                                 ;;(sb-c:source-location))))))
				 (or *compile-file-pathname* *load-pathname*))))))



(defmethod print-object ((method-combination standard-method-combination) stream)
  (print-unreadable-object (method-combination stream :identity t)
    (format stream "~A ~S" (class-name (class-of method-combination))
            (ignore-errors (mop::method-combination-name method-combination))))
  method-combination)


;; accessor functions, redefined from clos.lisp
(defun method-combination-name (method-combination)
  (check-type method-combination standard-method-combination)
  (std-slot-value method-combination 'sys::name))

(defun method-combination-documentation (method-combination)
  (check-type method-combination standard-method-combination)
  (std-slot-value method-combination 'sys:%documentation))

(defun short-method-combination-operator (method-combination)
  (check-type method-combination short-method-combination)
  (std-slot-value method-combination 'operator))

(defun short-method-combination-identity-with-one-argument (method-combination)
  (check-type method-combination short-method-combination)
  (std-slot-value method-combination 'identity-with-one-argument))

(defun long-method-combination-lambda-list (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'lambda-list))

;; how does this work?
(defun long-method-combination-method-group-specs (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'method-group-specs))

(defun long-method-combination-args-lambda-list (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination '%args-lambda-list))

(defun long-method-combination-generic-function-symbol (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination '%function))

(defun long-method-combination-function (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'function))

(defun long-method-combination-arguments (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'arguments))
;; ========================
;; Infrastructure Injection
;; ========================

;; #### NOTE: here, we need to take care of converting the two early method
;; combinations that were defined in the bootstrap phase (STANDARD and OR).
;; -- didier

(defun substitute-method-combination (new old)
  "Substitute NEW for OLD method combination.
OLD is an early method combination object (either the STANDARD or the OR one).
NEW is the corresponding full-blown object in the complete infrastructure.
This function transfers the generic functions cache from the old to the new
object, and updates all such generic functions to point to the new method
combination object."
  (setf (slot-value new '%generic-functions)
        ;;(method-combination-%generic-functions old))
	;; this probably doesnt work
	(long-method-combination-generic-function-symbol old))
  
  (map-hashset (lambda (gf)
                 (setf (generic-function-method-combination gf) new))
               ;;(method-combination-%generic-functions new)))
	       (long-method-combination-generic-function-symbol old)))


;; ---------------------------
;; Standard method combination
;; ---------------------------

(let* ((class (find-class 'standard-standard-method-combination))
       (instance (make-instance class)))
  (setf (slot-value class 'type-name) 'standard)
  (setf (slot-value class '%constructor)
        (lambda (options)
          (when options
            (method-combination-error
             "The standard method combination accepts no options."))
          instance))
  (setf (gethash nil (method-combination-type-%cache class)) instance)
  (setf (gethash 'standard **method-combination-types**) class)

  ;; changed global var names -- Julius
 ;; (substitute-method-combination instance +the-standard-method-combination+)
  (defconstant +the-standard-method-combination+ instance)
  (setf (get 'standard 'method-combination-object) +the-standard-method-combination+))


;; ------------------------------------
;; Built-in (short) method combinations
;; ------------------------------------

;;; The built-in method combination types as taken from page 1-31 of 88-002R.

;(define-method-combination +      :identity-with-one-argument t)
;(define-method-combination and    :identity-with-one-argument t)
;(define-method-combination append :identity-with-one-argument nil)
;(define-method-combination list   :identity-with-one-argument nil)
;(define-method-combination max    :identity-with-one-argument t)
;(define-method-combination min    :identity-with-one-argument t)
;(define-method-combination nconc  :identity-with-one-argument t)
;(define-method-combination progn  :identity-with-one-argument t)
;(define-method-combination or     :identity-with-one-argument t)

;(let* ((or-class (find-method-combination-type 'or))
;       (or-instance (funcall (method-combination-%constructor or-class)
;                      '(:most-specific-first))))
;  (setf (gethash '(:most-specific-first)
;                 (method-combination-type-%cache or-class))
;        or-instance)

  ;; TODO find equivalents in ABCL -- Julius
  ;;(substitute-method-combination or-instance *or-method-combination*)
  ;;(setq *or-method-combination* or-instance)
  
