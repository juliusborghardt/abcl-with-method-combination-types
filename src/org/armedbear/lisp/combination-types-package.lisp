;; This file is part of the Implementation of
;; Method Combination Types as proposed by Didier Verna
;; in HAL Id: hal-04751233 https://hal.science/hal-04751233v1
;; adapted for ABCL by Julius Borghardt https://github.com/juliusborghardt

(defpackage :method-combination-types
  (:use :cl :cl-user #-lispworks :mop #+lispworks :clos)
  (:export :find-method-combination*
   :find-method-combination :define-method-combination
   :method-combination-type-name :substitute-method-combination
   :standard-method-combination :method-combination-type
   :standard-method-combination-type :long-method-combination
   :short-method-combination :short-method-combination-type
   :long-method-combination-type :standard-standard-method-combination
   :**method-combination-types** :find-method-combination-type))

