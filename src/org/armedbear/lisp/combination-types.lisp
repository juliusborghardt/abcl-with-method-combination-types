;; The main file of the MOP-based method combination system for ABCL
;; by Julius Borghardt, based on a former implementation by Didier Verna

;; :cl src/org/armedbear/lisp/combination-types-init.lisp



(in-package :mop)

(defmethod method-combination-%generic-functions
    ((combination standard-method-combination))
  (slot-value combination '%generic-functions))

(defmethod standard-method-generic-functions
    ((combination standard-method-combination))
  (slot-value combination '%generic-functions))

(defmethod method-combination-options
    ((combination standard-method-combination))
  (slot-value combination 'options))

(defmethod method-combination-options
    ((combination t))
  (slot-value combination 'options))




(in-package :mop)
(defclass short-method-combination (standard-method-combination)
  ((operator :initarg :operator)
   (identity-with-one-argument :initarg :identity-with-one-argument)))

(defclass long-method-combination (standard-method-combination)
  ((sys::lambda-list :initarg :lambda-list)
   (method-group-specs :initarg :method-group-specs)
   (args-lambda-list :initarg :args-lambda-list)
   (generic-function-symbol :initarg :generic-function-symbol)
   (function :initarg :function)
   (arguments :initarg :arguments)
   (declarations :initarg :declarations)
   (forms :initarg :forms)))

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

;; allow class structure definition
(defmethod validate-superclass ((x standard-method-combination)
                                (y standard-class))
  t)

(defmethod validate-superclass ((x standard-method-combination-type)
                                (y standard-class))
  t)

(defmethod validate-superclass ((x standard-class)
                                (y method-combination-type))
  t)

(defmethod validate-superclass ((x method-combination-type)
                                (y standard-class))
  t)



;;validate instances as MCs
(defmethod validate-superclass ((new standard-method-combination)
                                (old method-combination))
  t)
(defmethod validate-superclass ((new short-method-combination)
                                (old (eql (find-class 'method-combination))))
  t)
(defmethod validate-superclass ((new long-method-combination)
                                (old (eql (find-class 'method-combination))))
  t)
(defmethod validate-superclass ((new (eql (find-class 'method-combination)))
                                (old standard-method-combination))
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

((eval-when (:load-toplevel :execute)
   (setf *standard-method-combination*
         (make-instance 'standard-standard-method-combination)))

;; (inspect **method-combination-types**)
 (defvar **method-combination-types** nil)




 (defparameter **method-combination-types** (make-hash-table :test 'eq)
   "The global method combination types hash table.
This hash table maps names to method combination types.")

 (defun find-method-combination-type (name &optional (errorp t))
   "Find a NAMEd method combination type.
If ERRORP (the default), throw an error if no such method combination type is
found. Otherwise, return NIL."
   ;; Test is 'eq, so symbols from different packages, like the test package, are not found!
   (or (gethash name **method-combination-types**)

       (loop for key being the hash-keys of **method-combination-types**
	     for value being the hash-values of **method-combination-types**
	     when (string= (string key) (symbol-name name))
	       return value)
	
       (when errorp
         (error "There is no method combination type named ~A." name))))


 (defmethod find-method-combination
     ((generic-function generic-function) name options)
   "Find a method combination object for type NAME and options.
If no method combination type exists by that NAME, return NIL.
Otherwise, a (potentially new) method combination object is returned.
The GENERIC-FUNCTION argument is ignored."
   (let ((type (or (find-method-combination-type name nil)

		   ;;Fallback: ignore packages and search by name
		   (loop for key being the hash-keys of **method-combination-types**
			 for value being the hash-values of **method-combination-types**
			 when (string= (string key) (symbol-name name))
			   return value))))
     (when type
       (or (gethash options (method-combination-type-%cache type))
           (setf (gethash options (method-combination-type-%cache type))
                 (funcall (method-combination-%constructor type)
			  options))))))

 #|
(defmethod find-method-combination
    ((generic-function generic-function) name options)
  ;; ABCL bug workaround:
  ;; Sometimes OPTIONS is the method-combination-type object itself.
  (format *error-output* "options: ~a" options)

  (let ((type (or (find-method-combination-type name nil)
                  (loop for key being the hash-keys of **method-combination-types**
                        for value being the hash-values of **method-combination-types**
                        when (string= (string key) (symbol-name name))
                          return value))))
    (when type
      (let* ((opts (normalize-mc-options options))
             (cache (method-combination-type-%cache type))
             (cached (gethash opts cache)))
        (or cached
            (setf (gethash opts cache)
                  (funcall (method-combination-%constructor type)
opts)))))))
|#


#|
(defmethod find-method-combination
    ((generic-function null) name options)
  ;; ABCL bug workaround:
  ;; Sometimes OPTIONS is the method-combination-type object itself.
  (format *error-output* "options: ~a" options)

  (let ((type (or (find-method-combination-type name nil)
                  (loop for key being the hash-keys of **method-combination-types**
                        for value being the hash-values of **method-combination-types**
                        when (string= (string key) (symbol-name name))
                          return value))))
    (when type
      (let* ((opts (normalize-mc-options options))
             (cache (method-combination-type-%cache type))
             (cached (gethash opts cache)))
        (or cached
            (setf (gethash opts cache)
                  (funcall (method-combination-%constructor type)
                           opts)))))))
|#



(defmethod find-method-combination
    ((generic-function null) name options)
  "Find a method combination object for type NAME and options.
If no method combination type exists by that NAME, return NIL.
Otherwise, a (potentially new) method combination object is returned.
The GENERIC-FUNCTION argument is ignored."
  (let ((type (or (find-method-combination-type name nil)

		  ;;Fallback: ignore packages and search by name
		  (loop for key being the hash-keys of **method-combination-types**
			for value being the hash-values of **method-combination-types**
			when (string= (string key) (symbol-name name))
			  return value))))
     (when type
       (or (gethash options (method-combination-type-%cache type))
          (setf (gethash options (method-combination-type-%cache type))
                (funcall (method-combination-%constructor type)
options)))))))


(defun find-method-combination* (name &optional options)
  (let ((type (find-method-combination-type name nil)))
    (when type
      (or (gethash options (method-combination-type-%cache type))
          (setf (gethash options (method-combination-type-%cache type))
                (funcall (method-combination-%constructor type)
                  options))))))

(defmethod update-generic-function-for-redefined-method-combination
    ((function generic-function)
     (previous standard-method-combination)
     (current standard-method-combination))
  "Flush the effective method cache and reinitialize FUNCTION."
  (flush-effective-method-cache function)
  (reinitialize-instance function))

(defmethod update-instance-for-different-class :after
    ((previous standard-method-combination)
     (current standard-method-combination)
     &key &allow-other-keys)
  "Inform every function using CURRENT method combination that it has changed."
  (maphash
   (lambda (gf _)
     (update-generic-function-for-redefined-method-combination
      gf previous current))
   (method-combination-%generic-functions current)))

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

(defmethod method-combination-type-name
    ((combination method-combination))
  "Return method COMBINATION's type name."
  (method-combination-type-name (class-of combination)))


;; already exists
#+nil(defmethod method-combination-name
    ((combination method-combination))
  "Return method COMBINATION's type name."
  (method-combination-type-name (class-of combination)))


(defmethod method-combination-type-name
    ((combination method-combination))
  "Return method COMBINATION's type name."
  (method-combination-type-name (class-of combination)))

(defmethod method-combination-lambda-list
    ((combination standard-method-combination))
  "Return method COMBINATION's lambda-list."
  (method-combination-type-lambda-list (class-of combination)))

;; exists
#+nil(defmethod short-method-combination-operator
    ((combination short-method-combination))
  "Return short method COMBINATION's operator."
  (short-method-combination-type-operator (class-of combination)))

#+nil(defmethod short-method-combination-identity-with-one-argument
    ((combination short-method-combination))
  "Return short method COMBINATION's identity-with-one-argument."
  (short-method-combination-type-identity-with-one-argument
   (class-of combination)))

#+nil(defmethod long-method-combination-%args-lambda-list
    ((combination long-method-combination))
  "Return long method COMBINATION's args-lambda-list."
  (long-method-combination-type-%args-lambda-list (class-of combination)))

#+nil(defmethod long-method-combination-function ((combination method-combination))
  (long-method-combination-type-%function (class-of combination)))


#+nil(defun long-method-combination-function (method-combination)
  (long-method-combination-type-%function (class-of method-combination)))

#+nil(defmethod long-method-combination-function ((combination long-method-combination-type))
  (long-method-combination-type-%function (class-of combination)))


#+nil(defmethod method-function ((combination long-method-combination-type))
  (long-method-combination-type-%function combination))

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
  (unless (listp options)
    (error "Illegal: :options is not a list but: ~a" options))
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
		    :class-name name
		    :metaclass mct-class
                    :direct-superclasses (list mc-class)
                    :documentation documentation
                    :type-name name
                    :operator operator
                    :identity-with-one-argument identity-with-one-argument
                    (when (consp mct-spec) (cdr mct-spec)))))

    #|
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
		     new :options (cond
				    ((null options)
				     '(:most-specific-first))
				    ((atom options)
				     (list options))
				    (t
    options)))))
    |#

    
    (setf (slot-value new '%constructor)
      (lambda (&optional options)
        (let* ((inst (make-instance new
                                    :options (or options '(:most-specific-first)))))
	  
          ;; Sync: legacy operator slot for old ABCL code
          (setf (slot-value inst 'operator)
                (short-method-combination-type-operator (class-of inst)))
	  (setf (slot-value inst 'identity-with-one-argument)
		(short-method-combination-type-identity-with-one-argument (class-of inst)))
          inst)))

    
    (load-defcombin name new documentation)))


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



#+nil(defmethod compute-primary-methods ((gf generic-function)
                                    (combin short-method-combination)
                                    applicable-methods)
  (let ((type-name (method-combination-type-name combin)))
    (remove-if-not (lambda (m) (let ((qs (method-qualifiers m)))
                                 (and (eql (car qs) type-name)
                                      (null (cdr qs)))))
                   applicable-methods)))

;; same but with looser test
(defmethod compute-primary-methods ((gf generic-function)
                                    (combin short-method-combination)
                                    applicable-methods)
  (let* ((type-name (method-combination-type-name combin))
         (type-name-string (symbol-name type-name)))
    (remove-if-not
     (lambda (m)
       (let ((qs (method-qualifiers m)))
         (and qs
              (string= (symbol-name (car qs)) type-name-string)
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
      ;; Return the load form
      `(load-long-defcombin
        ',type-name ',documentation #',function ',lambda-list
        ',args-option ',mc-class ',mct-spec))))



(defmethod slot-unbound
    ((class t)
     (obj long-method-combination)
     (slot-name (eql 'function)))
  (format *error-output*
          "~&[MC DEBUG] FUNCTION slot unbound on ~S (class ~S)~%"
          obj (class-of obj))
  (format *error-output* "~&---- ABCL BACKTRACE ----~%")
  ;; Safely print a backtrace without interfering with ABCL's own error
  (ignore-errors (sys::%backtrace *error-output*))
  (format *error-output* "~&---- END BACKTRACE ----~%")
  ;; Continue to normal UNBOUND-SLOT handler:
  (call-next-method))


(defun load-long-defcombin
    (name documentation function lambda-list args-lambda-list
          mc-class mct-spec
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
		    ;;:class-name nil ;;anonymous!!
		    :metaclass mct-class
		    
                    :direct-superclasses (list mc-class)
                    :documentation documentation
                    :type-name name
                    :lambda-list lambda-list
                    :args-lambda-list args-lambda-list
                    :function function
                    (cdr mct-spec))))



    
    (setf (slot-value new '%constructor)
	  (lambda (&optional options)
	    (format *error-output* "options: ~a" options)
        (let* ((inst (apply #'make-instance new
                            ;;:options options
			    )))
			    ;;:options options)))
          ;; Sync: legacy slots for old ABCL code
	  (setf (slot-value inst 'options)
		options)
          (setf (slot-value inst 'function)
                (long-method-combination-type-%function (class-of inst)))
          (setf (slot-value inst 'arguments)
                options)
          inst)))
    
    
    ;; no given options creates odd args list
    ;; this is a dirty fix, but it works
    (load-defcombin name new documentation)))



;; for future compatibility
(defun %program-error (fmt &rest args)
  (apply #'error 'program-error :format-control fmt :format-arguments args))


#|
(defmethod compute-effective-method
    ((function generic-function)
     (combination long-method-combination)
     applicable-methods)
  "Call the long method COMBINATION type's specific function."
  (funcall (long-method-combination-type-%function (class-of combination))
function combination applicable-methods))
|#

#|
(defmethod compute-effective-method
    ((function generic-function)
     (combination long-method-combination)
     applicable-methods)
  "Call the long method COMBINATION type's specific function."
  (funcall (long-method-combination-type-%function (class-of combination))
function combination applicable-methods))
|#

(defmethod compute-effective-method
    ((gf generic-function)
     (combination long-method-combination)
     applicable-methods)
  (funcall (long-method-combination-type-%function (class-of combination))
           gf combination applicable-methods))


(defmethod compute-effective-method
    ((gf generic-function)
     (combination long-method-combination)
     applicable-methods)
  (funcall (long-method-combination-type-%function (class-of combination))
           gf combination applicable-methods))

(defmethod compute-effective-method
    ((gf generic-function)
     (combination long-method-combination)
     applicable-methods)
  (funcall (long-method-combination-type-%function (class-of combination))
           gf combination applicable-methods))


(defun make-long-method-combination-function
    (type-name lambda-list method-group-specifiers args-option gf-var body)
  (declare (ignore type-name))
  (multiple-value-bind (real-body declarations documentation)
      (parse-body* body t)
    (let ((wrapped-body
            (wrap-method-group-specifier-bindings
             method-group-specifiers declarations real-body)))
      ;; optional 
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

(defun %make-long-method-combination-function
    (type-name lambda-list method-group-specifiers args-option gf-var body)
  (declare (ignore type-name))
  (multiple-value-bind (real-body declarations documentation)
      (parse-body* body t)
    (let ((wrapped-body
            (wrap-method-group-specifier-bindings
             method-group-specifiers declarations real-body)))
      ;; optional 
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

(defun constant-form-value (form)
  ;; adapted from SBCL
  (cond
    ;; literal self-evaluating objects
    ((not (symbolp form)) form)

    ;; keywords are constants
    ((keywordp form) form)

    ;; NIL and T
    ((eq form nil) nil)
    ((eq form t) t)

    ;; quoted form: (quote X) or 'X
    ((and (consp form)
          (eq (car form) 'quote)
          (consp (cdr form))
          (null (cddr form)))
     (cadr form))

    (t
     (error "Not a portable constant form: ~S" form))))


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
                 (order-value (and order-constantp
                                   (constant-form-value order))))
            (push name names)
            (push specializer-cache specializer-caches)
            (unless order-constantp
              (push `(,order-var ,order) order-vars))
            (let ((order-matters-test
                    (cond
                      ((and (eq (cadr method-group-specifier) '*)
                            (= nspecifiers 1))
                       nil)
                      (order-constantp (not (eql order-value nil)))
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


;; like in sbcl
(defun memq (e l)
  (do ((current l (cdr current)))
      ((atom current) nil)
    (when (eq (car current) e) (return current))))

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



;; v2 
(defmacro define-method-combination (&whole form name . args)
  (declare (ignore args name))
  `(progn
     ,(if (and (cddr form) (listp (caddr form)))
        (expand-long-defcombin form)
        (let* ((type-name (cadr form))
               (ioa (getf (cddr form) :identity-with-one-argument nil))
               (operator (getf (cddr form) :operator type-name))
               (mc-class (getf (cddr form) :method-combination-class
                               'short-method-combination))
               (mct-class (getf (cddr form) :method-combination-type-class
                                'short-method-combination-type)))
          `(load-short-defcombin ',type-name ',operator ',ioa
                                 ,nil
                                 ',mc-class ',mct-class)))))


#|    ;;not needed anymore, keeping for now
(defun substitute-method-combination (new old)
  "Transfer the generic-function cache from OLD to NEW and update all
affected generic functions."
  ;; currently broken!!
  
  ;; copy the cache
  (setf (slot-value new '%generic-functions)
        (slot-value old '%generic-functions))
  ;; update each generic function to use the new combination
  (maphash (lambda (gf _)
             (declare (ignore _))
             (setf (generic-function-method-combination gf) new))
           (slot-value new '%generic-functions)))
|#

(defun my-long-mc-instance-p (mc)
  (typep (class-of mc) 'long-method-combination-type))


;; does nothing
(defun std-compute-effective-method (gf method-combination methods)
  (assert (typep method-combination 'method-combination))
  (let* ((mc-name (or (method-combination-name method-combination)
		      (method-combination-type-name method-combination)))
	 ;; we cannot check the new system via name!!
	 
         (options (slot-value method-combination 'options))
         (order (car options))
         (primaries '())
         (arounds '())
         around
         emf-form
         (long-method-combination-p
          (typep method-combination 'long-method-combination)))
    (unless long-method-combination-p
      (dolist (m methods)
        (let ((qualifiers (method-qualifiers m)))
          (cond ((null qualifiers)
                 (if (eq mc-name 'standard)
                     (push m primaries)
                     (error "Method combination type mismatch: missing qualifier for method combination ~S." method-combination)))
                ((cdr qualifiers)
                 (error "Invalid method qualifiers: got a list."))
                ((eq (car qualifiers) :around)
                 (push m arounds))
                ((eq (car qualifiers) mc-name)
                 (push m primaries))
                ((memq (car qualifiers) '(:before :after)))
                (t
                 (error "Invalid method qualifiers: std-compute-effective-method found no valid qualifiers..~% qualifiers = ~a~%mc-name = ~a~% m = ~a~%" qualifiers mc-name m))))))
    (unless (eq order :most-specific-last)
      (setf primaries (nreverse primaries)))
    (setf arounds (nreverse arounds))
    (setf around (car arounds))
    (when (and (null primaries) (not long-method-combination-p))
      (error "No primary methods for the generic function ~S." gf))
    (cond
      (around
       (let ((next-emfun
              (funcall
               (if (std-generic-function-p gf)
                   #'std-compute-effective-method
                   #'compute-effective-method)
               gf method-combination (remove around methods))))
         (setf emf-form
               (generate-emf-lambda (method-function around) next-emfun))))
      ((eq mc-name 'standard)
       (let* ((next-emfun (compute-primary-emfun (cdr primaries)))
              (befores (remove-if-not #'before-method-p methods))
              (reverse-afters
               (reverse (remove-if-not #'after-method-p methods))))
         (setf emf-form
               (cond
                 ((and (null befores) (null reverse-afters))
                  (let ((fast-function (std-method-fast-function (car primaries))))
                    (if fast-function
                        (ecase (length (generic-function-required-arguments gf))
                          (1
                           #'(lambda (args)
                               (declare (optimize speed))
                               (funcall fast-function (car args))))
                          (2
                           #'(lambda (args)
                               (declare (optimize speed))
                               (funcall fast-function (car args) (cadr args)))))
                        (generate-emf-lambda (std-method-function (car primaries))
                                             next-emfun))))
                 (t
                  (let ((method-function (method-function (car primaries))))
                    #'(lambda (args)
                        (declare (optimize speed))
                        (dolist (before befores)
                          (funcall (method-function before) args nil))
                        (multiple-value-prog1
                            (funcall method-function args next-emfun)
                          (dolist (after reverse-afters)
                            (funcall (method-function after) args nil))))))))))
      (long-method-combination-p
       (error "~a" (my-long-mc-instance-p method-combination))
       #++(if (my-long-mc-instance-p method-combination)
	   (let ((fun (long-method-combination-function method-combination))
             (setf emf-form
		   (funcall fun gf method-combination methods))))

	   ;; → Fall back to ABCL legacy behaviour
	   (let ((fun (slot-value method-combination 'function))
		 (args (slot-value method-combination 'arguments)))
             (setf emf-form
		   (if args
                       (apply fun gf methods args)
                       (funcall fun gf methods))))))




      (t
       (let ((operator (short-method-combination-operator method-combination))
             (ioa (short-method-combination-identity-with-one-argument method-combination)))
         (setf emf-form
               (if (and ioa (null (cdr primaries)))
                   (generate-emf-lambda (method-function (car primaries)) nil)
                   `(lambda (args)
                      (,operator ,@(mapcar
                                    (lambda (primary)
                                      `(funcall ,(method-function primary) args nil))
                                    primaries))))))))
    (assert (not (null emf-form)))
    (or #+nil (ignore-errors (autocompile emf-form))
        (coerce-to-function emf-form))))
