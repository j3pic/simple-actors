(defpackage #:better-handler-case
  (:use #:common-lisp)
  (:nicknames #:handler-case*)
  (:export #:handler-case*))

(in-package #:better-handler-case)

(defun mapapply (func list)
  "Maps over a list, treating each element in the list as an argument list for FUNC."
  (mapcar (lambda (arglist)
	    (apply func arglist)) list))
  
(defun expand-handler-bind (type lambda-list &key (before-unwind nil supplied-p) after-unwind)
    "Helper for the HANDLER-CASE* macro. This one creates the HANDLER-BIND lambdas out of the :BEFORE-UNWIND form."
    (declare (ignorable after-unwind))
    (when supplied-p
      (when (null lambda-list)
	(error ":BEFORE-UNWIND must bind the condtion to a variable."))
      `(,type (lambda ,lambda-list (declare (ignorable ,@lambda-list)) ,before-unwind))))
  
(defun expand-handler-case (type lambda-list &key before-unwind (after-unwind nil has-after-unwind-p))
  "Helper for the HANDLER-CASE* macro. This one creates the HANDLER-CASE handler-clauses out of the :AFTER-UNWIND form"
  (declare (ignorable before-unwind))
  (if has-after-unwind-p
      `(,type ,lambda-list (declare (ignorable ,@lambda-list)) ,after-unwind)))

(defmacro handler-case* (form &rest cases)
  "Like HANDLER-CASE and HANDLER-BIND rolled into one. Example usage:
 
      (handler-case* (restart-case (error \"ZOMG! ERROR!\")
                         (fuck-it () 'ignored))
         (t (condition)
            :before-unwind
               (progn (format t \"An error occurred Ignore it (y/N)? \")
                      (if (eq (read) 'y)
                          (invoke-restart 'fuck-it)))
            :after-unwind
               (format t \"You just couldn't fucking ignore it, could you?~%\")))
     
:before-unwind is, of course, executed before the stack unrolls, so you can
invoke restarts from there. If no restart is invoked, the error falls through
to the :after-unwind case, where you can handle it like a regular handler-case.

If no :after-unwind form is provided and no restart is invoked, the condition is not trapped."
  `(handler-case
       (handler-bind ,(remove nil (mapapply #'expand-handler-bind cases))
	 ,form)
     ,@(remove nil (mapapply #'expand-handler-case cases))))

