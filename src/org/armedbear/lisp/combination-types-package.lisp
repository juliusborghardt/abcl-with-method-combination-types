;; This file is part of the Implementation of
;; Method Combination Types as proposed by Didier Verna
;; in HAL Id: hal-04751233 https://hal.science/hal-04751233v1
;; adapted for ABCL by Julius Borghardt https://github.com/juliusborghardt

(defpackage :method-combination-types
  (:use :cl :mop)
  (:import-from :mop
   :funcallable-standard-class
		:generic-function-method-combination
   :long-method-combination)
  (:export :find-method-combination* :change-method-combination
	   :define-medium-method-combination-type
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro
   :find-method-combination :define-method-combination
   :method-combination-type-name :substitute-method-combination
   :standard-method-combination :method-combination-type
   :standard-method-combination-type :long-method-combination
   :short-method-combination :short-method-combination-type
   :long-method-combination-type :standard-standard-method-combination))

