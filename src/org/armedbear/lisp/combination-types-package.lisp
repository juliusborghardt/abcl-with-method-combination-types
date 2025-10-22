(defpackage :method-combination-types
  (:use :cl)
  (:import-from :mop
    :funcallable-standard-class
    :generic-function-method-combination)
  (:import-from :clos
    :find-method-combination-type
    :long-method-combination
    :long-method-combination-type
    :update-generic-function-for-redefined-method-combination)
  (:export :find-method-combination* :change-method-combination
	   :define-medium-method-combination-type
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro))
