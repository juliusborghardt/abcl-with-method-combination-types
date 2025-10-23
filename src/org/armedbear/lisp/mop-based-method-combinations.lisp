;; An Implementation of Didier Verna's proposed method combination system
;; as seen in HAL Id: hal-04751233 https://hal.science/hal-04751233v1
;; by Julius Borghardt https://github.com/juliusborghardt



;;(require "COMPILE-FILE")
;;(load (do-compile "combination-types-package.lisp"))

(in-package :method-combination-types)


#|
(defclass standard-method-combination (metaobject)
  ((options :accessor standard-method-combination-options
	    :initarg :options
	    :initform nil)
   (%generic-functions
    :accessor standard-method-combination-generic-functions
    :initarg :generic-functions
    :initform nil)))

(defclass short-method-combination (standard-method-combination) ())
(defclass long-method-combination (standard-method-combination) ())



;; these are added in as meta classes
(defclass method-combination-type () ())
(defclass standard-method-combination-type (method-combination-type)
  ((type-name :accessor standard-method-combination-type-type-name
	      :initarg :type-name
	      :initform nil)
   (lambda-list :accessor standard-method-combination-type-lambda-list
		:initarg :lambda-list
		:initform nil)
   (%constructor :accessor standard-method-combination-type-%constructor
		 :initarg :%constructor
		 :initform nil)
   (%cache :accessor standard-method-combination-type-%cache
	   :initarg :%cache
	   :initform (make-hash-table :test 'string-equal))))


(defclass short-method-combination-type (standard-method-combination-type)
  ((operator
    :accessor short-method-combination-type-operator
    :initarg :operator
    :initform nil)
   (identity-with-one-argument
    :accessor short-method-combination-type-identity-with-one-argument
    :initarg :identity-with-one-argument
    :initform nil)))

(defclass long-method-combination-type (standard-method-combination-type)
  ((%args-lambda-list
    :accessor long-method-combination-type-%args-lambda-list
    :initarg :%args-lambda-list
    :initform nil)
   (%function :accessor long-method-combination-type-%function
	      :initarg :%function
	      :initform nil)))

(load (do-compile "medium.lisp"))
#|
;; instead load medium.lisp?
(defclass medium-method-combination-type (long-method-combination-type)
  ((operator :accessor medium-method-combination-type-operator
	     :initarg :operator
	     :initform nil)
   (identity-with-one-argument
    :accessor medium-method-combination-type-identity-with-one-argument
    :initarg :identity-with-one-argument
:initform nil)))
|#

;; singleton standard meth com, the one instance of this will be *standard-method-combination*
;; order of class options matters!
(defclass standard-standard-method-combination (standard-method-combination)
  ((type-name
    :accessor standard-standard-method-combination-type-name
    :initarg :type-name
    :initform "standard"))
  (;;:optimize-slot-access nil       needed?
   :metaclass standard-method-combination-type))
|#


;;(load (do-compile "defcombin.lisp"))
#|
;; redefined from clos.lisp
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
            (%program-error
             "~@<~S argument to the short form of ~S must be a string.~:@>"
             :documentation 'define-method-combination))
          `(load-short-defcombin ',type-name ',operator ',ioa
                                 ,(unless (unbound-marker-p doc) doc)
                                 ',mc-class ',mct-class
                                 (sb-c:source-location))))))



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
        (method-combination-%generic-functions old))
  (map-hashset (lambda (gf)
                 (setf (generic-function-method-combination gf) new))
               (method-combination-%generic-functions new)))


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
  (substitute-method-combination instance *standard-method-combination*)
  (setq *standard-method-combination* instance))


;; ------------------------------------
;; Built-in (short) method combinations
;; ------------------------------------

;;; The built-in method combination types as taken from page 1-31 of 88-002R.

(define-method-combination +      :identity-with-one-argument t)
(define-method-combination and    :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list   :identity-with-one-argument nil)
(define-method-combination max    :identity-with-one-argument t)
(define-method-combination min    :identity-with-one-argument t)
(define-method-combination nconc  :identity-with-one-argument t)
(define-method-combination progn  :identity-with-one-argument t)
(define-method-combination or     :identity-with-one-argument t)

(let* ((or-class (find-method-combination-type 'or))
       (or-instance (funcall (method-combination-%constructor or-class)
                      '(:most-specific-first))))
  (setf (gethash '(:most-specific-first)
                 (method-combination-type-%cache or-class))
        or-instance)
  (substitute-method-combination or-instance *or-method-combination*)
  (setq *or-method-combination* or-instance))
|#

;;(load (do-compile "util.lisp"))
#|
;; util
;; A better protocol to access method combination objects. This is merely a
;; duplication of my patched SBCL's code for FIND-METHOD-COMBINATION. There's
;; no point in implementing a SETF method here since those objects are handled
;; internally and automatically.
(defun find-method-combination*
    (name &optional options (errorp t)
	  &aux (type (find-method-combination-type name errorp)))
  "Find a method combination object for NAME and OPTIONS.
If ERRORP (the default), throw an error if no NAMEd method combination type is
found. Otherwise, return NIL. Note that when a NAMEd method combination type
exists, asking for a new set of (conformant) OPTIONS will always instantiate
the combination again, regardless of the value of ERRORP."
  (when type
    (or (gethash options (sb-pcl::method-combination-type-%cache type))
	(setf (gethash options (sb-pcl::method-combination-type-%cache type))
	      (funcall (sb-pcl::method-combination-%constructor type)
		options)))))

(defmacro change-method-combination (function &rest combination)
  "Change generic FUNCTION to a new method COMBINATION.
- FUNCTION is a generic function designator.
- COMBINATION is a method combination type name, potentially followed by
arguments."
  (when (symbolp function) (setq function `(function ,function)))
  `(reinitialize-instance ,function
     :method-combination
     (find-method-combination* ',(car combination) ',(cdr combination))))
|#



#|
;; Redefine the relevant ABCL globals:
;; +the-standard-method-class+ from clos.lisp
(defconstant +the-standard-method-combination+
  (let ((instance (std-allocate-instance (find-class 'standard-standard-method-combination))))
    
    ;; (setf (std-slot-value instance 'sys::name) 'standard)
    ;; not needed, singleton gets this in initform
    
    (setf (std-slot-value instance 'sys:%documentation)
          "The standard method combination.")

    (setf (std-slot-value instance 'options) nil)
    instance)
  "The standard method combination.
Do not use this object for identity since it changes between
compile-time and run-time.  To detect the standard method combination,
compare the method combination name to the symbol 'standard.")
(setf (get 'standard 'method-combination-object) +the-standard-method-combination+)
|#

;; print


;(defun long-method-combination-declarations (method-combination)
;  (check-type method-combination long-method-combination)
;  (std-slot-value method-combination 'declarations))

;(defun long-method-combination-forms (method-combination)
;  (check-type method-combination long-method-combination)
;  (std-slot-value method-combination 'forms))
