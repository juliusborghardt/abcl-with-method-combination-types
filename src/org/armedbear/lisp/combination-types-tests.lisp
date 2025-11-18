;;;; ------------------------------------------------------------------
;;;; Test suite for portable method combination implementation
;;;; load with: :cl src/org/armedbear/lisp/combination-types-tests.lisp
;;;; (method-combination-tests:run-symbol-bound-check)
;;;; ------------------------------------------------------------------

(defpackage :method-combination-tests
  (:use :cl :mop :method-combination-types)
  (:export :run-method-combination-tests :run-symbol-bound-check
	   :run-long-combination-tests :run-short-combination-tests))
(in-package :method-combination-tests)

(defun log (fmt &rest args)
  (format t "~&[TEST] ~?~%" fmt args))

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (error "Assertion failed: expected ~S but got ~S" expected actual)))


;;; -------------------------------------------------------------------
;;; 0. Determine which MOP features are available
;;; -------------------------------------------------------------------

(defun run-symbol-bound-check ()
  (log "Starting check for bound symbols...")
  (loop for sym in  
	'(find-method-combination
	  compute-effective-method
	  reinitialize-instance
	  update-instance-for-different-class
	  add-method remove-method
	  shared-initialize
	  make-instance change-class)
do (log (format nil "~a" (list sym (fboundp sym)))))

  (loop for class in 
	  '(method-combination
	  standard-method-combination
	  short-method-combination
	  long-method-combination
	  generic-function standard-generic-function
	  standard-class metaobject)
	do (log (format nil "~a" (list class (find-class class nil))))))


;;; -------------------------------------------------------------------
;;; 1. Define a short and a long method combination
;;; -------------------------------------------------------------------
(define-method-combination my-sum
    (&optional (order :most-specific-first))
  ((around  (:around))
   (before  (:before) :order order)
   (primary ()        :order order :required t)
   (after   (:after)  :order order))
  ;; Body builds the effective method form:
  (let* ((before-form
          (if before
              `(+ ,@(mapcar (lambda (m) `(call-method ,m)) before))
              0))
         (primary-form
          (if (rest primary)
              `(+ ,@(mapcar (lambda (m) `(call-method ,m)) primary))
              `(call-method ,(first primary))))
         (after-form
          (if after
              `(+ ,@(mapcar (lambda (m) `(call-method ,m)) after))
              0))
         (combined
          `(+ ,before-form ,primary-form ,after-form)))
    (if around
        ;; Let the first :around method wrap the combined computation.
        ;; Remaining :around methods are passed as the method list,
        ;; and the inner computation is provided via MAKE-METHOD.
        `(call-method ,(first around)
                      (,@(rest around) (make-method ,combined)))
        combined)))



(define-method-combination my-max
  :operator max
  :identity-with-one-argument t)

;;; -------------------------------------------------------------------
;;; 2a. Tests for the long
;;; -------------------------------------------------------------------
(defgeneric test-sum (x y)
  (:method-combination my-sum))

;;; Primary methods
(defmethod test-sum ((x number) (y number))
  (+ x y))

;;; Around and before methods (should combine additively)
(defmethod test-sum :around ((x integer) (y integer))
  (+ 100 (call-next-method)))

(defmethod test-sum :before ((x integer) (y integer))
  1)

;;; Another applicable method
(defmethod test-sum ((x (eql :special)) (y number))
  (+ 1000 y))


;;; -------------------------------------------------------------------
;;; 2b. Tests for the short
;;; -------------------------------------------------------------------
(defgeneric test-max (x)
  (:method-combination my-max))

;; Multiple primary methods contributing different results
(defmethod test-max my-max ((x (eql :a))) 10)
(defmethod test-max my-max ((x (eql :b))) 42)
(defmethod test-max my-max ((x (eql :c))) 7)
(defmethod test-max my-max ((x t)) 5)

(defclass parent () ())
(defclass child (parent) ())

(defgeneric test-max-inheritance (obj)
  (:method-combination my-max))
    
(defmethod test-max-inheritance my-max ((o parent)) 10)
(defmethod test-max-inheritance my-max ((o child)) 99)
  
;;; -------------------------------------------------------------------
;;; 3. Run-time checks
;;; -------------------------------------------------------------------
(defun run-long-combination-tests ()
  (let ((l1 (test-sum 2 3))
	(l2 (test-sum :special 7)))

    ;; BEGIN LONG FORM TESTS
    (log "test-sum(2,3)  => ~A" l1)
    (log "test-sum(:special,7) => ~A" l2)
    ;; Expected results depend on additive combination:
    ;; 100 (around) + (2+3) from primary + 1 (before) = 106
    ;; the (after) method does not contribute its 2
    (assert-equal 106 l1)
    ;; :special method should run alone, 1000 + 7 = 1007
    (assert-equal 1007 l2)
    (log "Long-form cases OK.")))

(defun run-short-combination-tests ()
  (let ((s1 (test-max :a))
        (s2 (test-max :b))
        (s3 (test-max :c))
        (s4 (test-max :z)))
    (log "test-max(:a) => ~A" s1)
    (log "test-max(:b) => ~A" s2)
    (log "test-max(:c) => ~A" s3)
    (log "test-max(:z) => ~A" s4)
    (assert-equal 10 s1)
    (assert-equal 42 s2)
    (assert-equal 7  s3)
    (assert-equal 5  s4)
    (log "Single-method short-form cases OK.")

    (let ((result (test-max-inheritance (make-instance 'child))))
      (log "test-max-inheritance(child) => ~A" result)
      ;; Should apply MAX over 10 and 99 = 99
      (assert-equal 99 result)
      (log "Multiple applicable method short-form case OK."))))

(defun run-method-combination-tests ()
  (log "Running method-combination tests...")
  
    ;; BEGIN LONG FORM TESTS
    (run-long-combination-tests)

    ;; BEGIN SHORT FORM TESTS
    (run-short-combination-tests)
  
  (log "All tests passed successfully.")
      t)


