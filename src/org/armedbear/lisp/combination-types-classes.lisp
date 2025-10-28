(in-package :method-combination-types)

;; ========================
;; Method Combination Types
;; ========================

;; This file is part of the Implementation of
;; Method Combination Types as proposed by Didier Verna
;; in HAL Id: hal-04751233 https://hal.science/hal-04751233v1
;; adapted for ABCL by Julius Borghardt https://github.com/juliusborghardt

(defclass method-combination-type (standard-class)
  ()
  (:documentation "Metaclass for all method combination types."))

;(eval-when (:compile-toplevel :load-toplevel :execute)
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

  (defmethod validate-superclass
      ((class standard-method-combination-type) (superclass standard-class))
    "Validate the creation of subclasses of METHOD-COMBINATION implemented as
STANDARD-METHOD-COMBINATION-TYPE."
    t)

  ;; maybe symmetry is needed to guarantee compatability? --JB
  (defmethod validate-superclass
      ((class standard-class) (superclass standard-method-combination-type))
    t)
;  )



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


;; this fails

(defclass standard-standard-method-combination (standard-method-combination)
    ()
    (:metaclass standard-method-combination-type)
    (:documentation "The standard method combination."))
