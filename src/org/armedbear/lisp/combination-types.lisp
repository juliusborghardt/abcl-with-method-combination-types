;; Didier's system as a non-internal package

(in-package :method-combination-types)

(defclass standard-method-combination (metaobject) 
  ((options :accessor standard-method-combination-options :initarg :options :initform nil)
   (%generic-functions :accessor standard-method-combination-generic-functions :initarg :generic-functions :initform nil)))


(defclass short-method-combination (standard-method-combination) ())
(defclass long-method-combination (standard-method-combination) ())



;; these are added in as meta classes
(defclass method-combination-type (standard-class) ())
(defclass standard-method-combination-type (method-combination-type)
  ((type-name :initarg :type-name :reader method-combination-type-name)
   (lambda-list :initform nil :initarg :lambda-list
                :reader method-combination-type-lambda-list)
   ;; A reader without "type" in the name seems more readable to me.
   (%constructor :reader method-combination-%constructor)
   (%cache :initform (make-hash-table :test #'equal)
           :reader method-combination-type-%cache))
  (:documentation "Metaclass for standard method combination types.
It is the base class for short and long method combination types metaclasses.
This only class directly implemented as this class is the standard method
combination class."))


#|
(defmethod initialize-instance :after ((obj standard-method-combination-type) &rest initargs &key &allow-other-keys)
  (setf (method-combination-%constructor obj) (lambda () (make-instance (class-name (class-of obj))))))
|#


(defmethod validate-superclass ((x standard-method-combination)
                                (y standard-class))
  t)

(defmethod validate-superclass ((x standard-method-combination-type)
                                (y standard-class))
  t)

(defclass short-method-combination-type (standard-method-combination-type)
  ((lambda-list :initform '(&optional (order :most-specific-first)))
   (operator :initarg :operator
             :reader short-method-combination-type-operator)
   (identity-with-one-argument
    :initarg :identity-with-one-argument
    :reader short-method-combination-type-identity-with-one-argument))
  (:documentation "Metaclass for short method combination types."))

(defclass long-method-combination-type (standard-method-combination-type)
  ((%args-lambda-list :initform nil :initarg :args-lambda-list
                      :reader long-method-combination-type-%args-lambda-list)
   (%function :initarg :function
              :reader long-method-combination-type-%function))
  (:documentation "Metaclass for long method combination types."))

(defclass medium-method-combination-type (long-method-combination-type)
  ((operator :accessor medium-method-combination-type-operator :initarg :operator :initform nil)
  (identity-with-one-argument :accessor medium-method-combination-type-identity-with-one-argument :initarg :identity-with-one-argument :initform nil)))

;; singleton standard meth com, the one instance of this will be *standard-method-combination*
;; order of class options matters!
(defclass standard-standard-method-combination (standard-method-combination)
  ((type-name :accessor standard-standard-method-combination-type-name
	      :initarg :type-name
	      :initform "standard"))
  (:metaclass standard-method-combination-type))
;; for non-standard meth-coms, the pendant to this class will be anonymous


;; defer instantiation to until after compile-time
(defvar *standard-method-combination* nil
  "The single standard method combination instance.")

(eval-when (:load-toplevel :execute)
  (defparameter *standard-method-combination* (make-instance 'standard-standard-method-combination)))

(eval-when (:load-toplevel :execute)
  (setf *standard-method-combination*
        (make-instance 'standard-standard-method-combination)))


;; needed bc. uninitialized slot error?
(defvar **method-combination-types** nil)

(eval-when (:load-toplevel :execute)
  (defparameter **method-combination-types** (make-hash-table :test 'eq)
    "The global method combination types hash table.
This hash table maps names to method combination types."))

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
  (declare (ignore documentation))
  (when old
    (setf (slot-value new '%cache) (method-combination-type-%cache old))
    (maphash (lambda (options combination)
               (declare (ignore options))
               (change-class combination new))
             (method-combination-type-%cache new)))
  (setf (gethash name **method-combination-types**) new)
  ;;(setf (random-documentation name 'method-combination) documentation)
  name)




;; -----------------------------------
;; Method combination pseudo-accessors
;; -----------------------------------


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

(defmethod compute-primary-methods
    ((gf generic-function)
     (combin standard-standard-method-combination)
     applicable-methods)
  (remove-if #'method-qualifiers applicable-methods))


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
                                   (car mct-spec)))))
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


#++(defmethod invalid-qualifiers
    ((gf generic-function) (combin short-method-combination) method)
  (let* ((qualifiers (method-qualifiers method))
         (qualifier (first qualifiers))
         (type-name (method-combination-type-name combin))
         (why (cond
                ((null qualifiers)
                 "has no qualifiers")
                ((cdr qualifiers)
                 "has too many qualifiers")
                (t(boundp 'method-combination-types:find
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

(defun expand-long-defcombin (form)
  (let ((type-name (cadr form))
        (lambda-list (caddr form))
        (method-group-specifiers-presentp (cdddr form))
        (method-group-specifiers (cadddr form))
        (body (cddddr form))
        (args-option nil)
        (gf-var nil)
        (mc-class 'long-method-combination)
        (mct-spec '(long-method-combination-type)))
    (unless method-group-specifiers-presentp
      (error "~@<The long form of ~S requires a list of method group specifiers.~:@>"
             'define-method-combination))
    ;; parse options
    (when (and (consp (car body)) (eq (caar body) :arguments))
      (setq args-option (cdr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :generic-function))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (error "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
               :generic-function 'define-method-combination))
      (setq gf-var (cadr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :method-combination-class))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (error "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
               :method-combination-class 'define-method-combination))
      (setq mc-class (cadr (pop body))))
    (when (and (consp (car body))
               (eq (caar body) :method-combination-type-class))
      (setq mct-spec (cdr (pop body))))
    (multiple-value-bind (documentation function)
        (make-long-method-combination-function
         type-name lambda-list method-group-specifiers args-option gf-var
         body)
      ;; Return the LOAD form
      `(load-long-defcombin
        ',type-name ',documentation #',function ',lambda-list
        ',args-option ',mc-class ',mct-spec nil)))) ; source-location omitted


(defun load-long-defcombin
    (name documentation function lambda-list args-lambda-list
          mc-class mct-spec source-location
          &aux (mc-class (find-class mc-class))
               (mct-class (find-class (car mct-spec))))
  ;; basic class checks
  (unless (subtypep mc-class 'long-method-combination)
    (error "Invalid method combination class: ~A.~%~
            When defining a method combination type in long form, the provided~%~
            method combination class must be a subclass of LONG-METHOD-COMBINATION."
           mc-class))
  (unless (subtypep mct-class 'long-method-combination-type)
    (error "Invalid method combination type class: ~A.~%~
            When defining a method combination type in long form, the provided~%~
            method combination type class must be a subclass of LONG-METHOD-COMBINATION-TYPE."
           mct-class))
  ;; Create the new method-combination-type instance
  (let ((new (apply #'make-instance mct-class
                    :source source-location
                    :direct-superclasses (list mc-class)
                    :documentation documentation
                    :type-name name
                    :lambda-list lambda-list
                    :args-lambda-list args-lambda-list
                    :function function
                    (cdr mct-spec))))
    ;; install constructor
    (setf (slot-value new '%constructor)
          (lambda (options)
            (apply #'make-instance new :options options)))
    (load-defcombin name new documentation)))



;; for future compatibility
(defun %program-error (fmt &rest args)
  (apply #'error 'program-error :format-control fmt :format-arguments args))


(defmethod compute-effective-method
    ((function generic-function)
     (combination long-method-combination)
     applicable-methods)
  "Call the long method COMBINATION type's specific function."
  (funcall (long-method-combination-type-%function (class-of combination))
    function combination applicable-methods))


(defun make-long-method-combination-function
    (type-name lambda-list method-group-specifiers args-option gf-var body)
  (declare (ignore type-name))
  ;; Use a portable PARSE-BODY
  (multiple-value-bind (real-body declarations documentation)
      (parse-body* body t)
    (let ((wrapped-body
            (wrap-method-group-specifier-bindings
             method-group-specifiers declarations real-body)))
      ;; optional generic-function variable binding
      (when gf-var
        (push `(,gf-var .generic-function.) (cadr wrapped-body)))
      ;; handle :arguments option
      (when args-option
        (setq wrapped-body
              (deal-with-args-option wrapped-body args-option)))
      ;; handle lambda-list wrapping
      (when lambda-list
        (setq wrapped-body
              `(apply (lambda ,lambda-list ,wrapped-body)
                      (method-combination-options .method-combination.))))
      ;; Return documentation and function form
      (values
       documentation
       `(lambda (.generic-function. .method-combination. .applicable-methods.)
          (declare (ignorable .generic-function.
                              .method-combination.
                              .applicable-methods.))
          (block .long-method-combination-function.
            ,wrapped-body))))))

(defun parse-body* (body &optional doc-string-allowed)
  "Return three values: body without declarations/doc,
   list of declarations, and the documentation string (or NIL)."
  (let ((declarations '())
        (documentation nil))
    ;; extract leading documentation string
    (when (and doc-string-allowed
               (stringp (first body)))
      (setf documentation (pop body)))
    ;; extract leading declarations
    (loop while (and (consp (first body))
                     (eq (caar body) 'declare))
          do (push (pop body) declarations))
    (values body (nreverse declarations) documentation)))

(define-condition long-method-combination-error
    (reference-condition simple-error)
  ()
  (:default-initargs
   :references '((:ansi-cl :macro define-method-combination))))

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


;; keep one?
(defun memq (e l)
  (do ((current l (cdr current)))
      ((atom current) nil)
    (when (eq (car current) e) (return current))))

#+nil(defun memq (item list)
  (member item list :test #'eq))

(defun parse-method-group-specifier (method-group-specifier)
  (unless (symbolp (car method-group-specifier))
    (%program-error "~@<Method group specifiers in the long form of ~S ~
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
      (%program-error "~@<Method group specifiers in the long form of ~S ~
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



(defun parse-optional-arg-spec (spec)
  "Return (values name default suppliedp)."
  (cond ((symbolp spec)
         (values spec nil nil))
        ((and (consp spec) (symbolp (first spec)))
         (values (first spec) (second spec) (third spec)))
        (t (error "Invalid optional arg spec: ~S" spec))))

(defun parse-key-arg-spec (spec)
  "Return (values keyword name default suppliedp)."
  (cond ((symbolp spec)
         (values (intern (string spec) :keyword) spec nil nil))
        ((and (consp spec)
              (symbolp (second spec)))
         (values (first spec) (second spec)
                 (third spec) (fourth spec)))
        (t (error "Invalid key arg spec: ~S" spec))))


(defun parse-lambda-list-simple (ll)
  "Just good enough for define-method-combination argument lambda-lists."
  (let ((required '())
        (optional '())
        (rest nil)
        (key '())
        (aux '())
        (whole nil)
        (mode :required))
    (dolist (elt ll)
      (case elt
        (&optional (setf mode :optional))
        (&rest     (setf mode :rest))
        (&key      (setf mode :key))
        (&aux      (setf mode :aux))
        (&whole    (setf mode :whole))
        (otherwise
         (ecase mode
           (:required (push elt required))
           (:optional (push elt optional))
           (:rest (setf rest (list elt))
                  (setf mode :after-rest))
           (:key (push elt key))
           (:aux (push elt aux))
           (:whole (setf whole (list elt)))))))
    (values (list required optional rest key aux nil whole)
            required optional rest key aux nil whole)))



(defun deal-with-args-option (wrapped-body args-lambda-list)
  (multiple-value-bind (llks required optional rest key aux env whole)
      (parse-lambda-list-simple args-lambda-list)
    (declare (ignore llks env))
    (let (intercept-rebindings)
      ;; collect gensym rebindings
      (flet ((intercept (sym) (push `(,sym ',sym) intercept-rebindings)))
        (when whole (intercept (car whole)))
        (dolist (arg required) (intercept arg))
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
          (intercept (if (consp arg) (car arg) arg))))
      (setf intercept-rebindings (nreverse intercept-rebindings))
      ;; Ensure WRAPPED-BODY is a LET-like form
      (assert (member (first wrapped-body) '(let let*) :test #'eq))
      ;; inject rebindings
      (setf (second wrapped-body)
            (append intercept-rebindings (second wrapped-body)))
      ;; fill out args-lambda-list if too short
      (unless (or (member '&rest args-lambda-list :test #'eq)
                  (member '&allow-other-keys args-lambda-list :test #'eq))
        (let ((auxpos (member '&aux args-lambda-list :test #'eq)))
          (setf args-lambda-list
                (append (ldiff args-lambda-list auxpos)
                        (if (member '&key args-lambda-list :test #'eq)
                            '(&allow-other-keys)
                            '(&rest .ignore.))
                        auxpos))))
      ;; construct final let/destructuring form
      `(let ((inner-result. ,wrapped-body)
             (gf-lambda-list (generic-function-lambda-list .generic-function.)))
         `(destructuring-bind ,',args-lambda-list
              (frob-combined-method-args
               .gf-args. ',gf-lambda-list
               ,',(length required) ,',(length optional))
            ,,(when (member '.ignore. args-lambda-list :test #'eq)
                ''(declare (ignore .ignore.)))
            ,,(when whole
                ``(setq ,',(car whole) .gf-args.))
            ,inner-result.)))))




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



(defmacro with-single-package-locked-error (options &body body)
  "Just incase this needs to exist"
  (declare (ignore options))
  `(progn ,@body))


(defmacro define-method-combination (&whole form name &rest args)
  "Define a :meth-comb method combination."
  (declare (ignore args)) ;; because whole form is passed to expander
  (unless (symbolp name)
    (error "DEFINE-METHOD-COMBINATION: name ~S must be a symbol." name))
  ;; Distinguish long vs. short form
  (if (and (cddr form) (listp (caddr form)))
      ;; Long form: delegate to  expander
      (expand-long-defcombin form)
      ;; Short form: parse the plist
      (let* ((plist    (cddr form))
             (doc-pair (member :documentation plist :test #'eq))
             (doc      (and doc-pair (cadr doc-pair)))
             (ioa-pair (member :identity-with-one-argument plist :test #'eq))
             (ioa      (and ioa-pair (cadr ioa-pair)))
             (op-pair  (member :operator plist :test #'eq))
             (operator (if op-pair (cadr op-pair) name))
             (mc-pair  (member :method-combination-class plist :test #'eq))
             (mc-class (if mc-pair (cadr mc-pair) 'short-method-combination))
             (mct-pair (member :method-combination-type-class plist :test #'eq))
             (mct-class (if mct-pair (cadr mct-pair) 'short-method-combination-type)))
        (when (and doc-pair (not (stringp doc)))
          (error "~@<~S argument to the short form of ~S must be a string.~:@>"
                 :documentation 'define-method-combination))
        ;; Expand to loader, pass nil for source-location.
        `(progn
           (load-short-defcombin ',name ',operator ',ioa
                                 ,(if doc-pair doc nil)
                                 ',mc-class ',mct-class
                                 nil)))))


(defun substitute-method-combination (new old)
  "Transfer the generic-function cache from OLD to NEW and update all
affected generic functions."
  ;; copy the cache
  (setf (slot-value new '%generic-functions)
        (slot-value old '%generic-functions))
  ;; update each generic function to use the new combination
  (maphash (lambda (gf _)
             (declare (ignore _))
             (setf (generic-function-method-combination gf) new))
           (slot-value new '%generic-functions)))
