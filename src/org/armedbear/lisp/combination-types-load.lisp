;; Part of the MOP-based method combination system implementation

;; *lisp-home* = #P"jar:file:///home/jb/Documents/Lisp/ABCL/abcl/dist/abcl.jar!/org/armedbear/lisp/"
#+nil(defun abcl-source-directory ()
  "find the full path to abcl/src/org/armedbear/lisp/."
  (let ((home (namestring *lisp-home*)))
    ;; running from jar
    (if (equalp (subseq home 0 3) "jar")
        (let* ((path (car (split "dist" (cadr (split "///" home))))))
          (merge-pathnames #P"src/org/armedbear/lisp/" path))
      
	;; not jar
        *lisp-home*)))




#+nil(defun install-mc-init-file (&key (overwrite nil))
  "Install .abclrc with method combination loader"
  (let* ((dir (abcl-source-directory)) 
         (package-file (merge-pathnames "combination-types-package.lisp" dir))
         (impl-file (merge-pathnames "combination-types.lisp" dir))
	 (init-file (merge-pathnames "combination-types-init.lisp" dir))
         (home         (user-homedir-pathname))
         (abclrc       (merge-pathnames ".abclrc" home)))

    (when (or overwrite (not (probe-file abclrc)))
      (with-open-file (out abclrc
                           :direction :output
                           :if-exists (if overwrite :supersede :error)
                           :if-does-not-exist :create)
        (format out "~&;; Auto-generated ABCL init file~%")
        (format out "(format t \"[Combination Types] Loading method combination system...~%\")~%")

	;;just the defpackage
        (format out "(load ~S)~%" (namestring package-file))(load "combination-types.lisp")

	;;the full implementation
        (format out "(load ~S)~%" (namestring impl-file))

	;;replace existing method combinations
	(format out "(load ~S)~%" (namestring init-file))
        (format out "(format t \"[Combination Types]...Done.~%\")")))
	
    abclrc))




;;(install-mc-init-file :overwrite t)



