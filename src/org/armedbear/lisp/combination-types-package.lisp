(format t "; attempting to load package :method-combination-types")
(defpackage :method-combination-types
  (:use :cl)
  (:import-from :mop
   :funcallable-standard-class
		:generic-function-method-combination
   :long-method-combination)
  (:export :find-method-combination* :change-method-combination
	   :define-medium-method-combination-type
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro))
(format t "; finished loading package :method-combination-types")
;;(in-package :method-combination-types) package isnt known yet
