(defpackage :method-combination-types
  (:use :cl)
  (:import-from :mop
   :funcallable-standard-class
		:generic-function-method-combination
   :long-method-combination)
  (:import-from :SYSTEM
		:aver)
  (:export :find-method-combination* :change-method-combination
	   :define-medium-method-combination-type
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro))
;;(in-package :method-combination-types) package isnt known yet
