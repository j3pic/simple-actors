(defpackage :utils
  (:use :closer-common-lisp :hof :net.telent.date)
  (:export :hash->alist :alist->hash :plist->alist :alist->plist :export-class :it :aif :acond :awhen :copy :join :raise-simple-error
	   :format-simple-error :format-error :insert-into-list :stringcase :estringcase :begins-with-p :split :group
	   :take :take-while :drop :drop-while :recursive-find/collect :recursive-find/replace :recursive-find :recursive-mapcar :sift
	   :setf-if-nil :positivep :negativep :pathname-append :to-string :destructuring-lambda
	   :char-range :let-values* :set-intersection :print-signs :foreach :named-let :fast-remove-duplicates :explode
	   :mapcar/append :last-element :with-retry :zero-pad :λ :file->string :integer->boolean :sort-by :*sort-by-eq*
	   :map-leaves :sql-timestamp->integer :string-or-null :split-when :map-values :apply-values :reverse-values :insert-before! :remove-at!
	   :split-at :with-gensyms :select-columns :string+= :debuggable-typecheck :get-restart :invoke-restart-by-name :only-once :pipe :grep))


(in-package :utils)

(defmacro with-gensyms (symbols &body body)
  `(let ,(loop for symb in symbols
	    collect `(,symb (gensym)))
     ,@body))

(defmacro only-once (symbol expression &body body)
  (with-gensyms (var)
    `(let* ((,var ,expression)
	   (,symbol ,var))
       ,@body)))

(defun hash->alist (hash)
  (let ((result nil))
    (maphash (lambda (k v)
	       (push (cons k v) result))
	     hash)
    result))

(defun alist->hash (alist &key (test 'eql))
  (let ((hash (make-hash-table :test test)))
    (loop for (k . v) in alist
       do (setf (gethash k hash) v))
    hash))

(defun plist->alist (plist)
  (loop for (key value) on plist by #'cddr
     collect (cons key value)))

(defun alist->plist (alist)
  (loop for (key . value) in alist
     collect key
     collect value))       

(defun export-class (class)
  (finalize-inheritance class)
  (loop for slot in (class-slots class)
       do (export (slot-value slot 'sb-pcl::name))))

(defmacro aif (cond if-true &optional if-false)
  `(let ((it ,cond))
     (if it
	 ,if-true
	 ,if-false)))

(defmacro acond (&rest conditions)
  (let ((result nil))
    (loop for (cond . body) in (reverse conditions)
       do (setf result
		`(aif ,cond (progn ,@body)
		      ,result)))
    result))

(defmacro awhen (condition &body body)
  `(aif ,condition
	(progn ,@body)))

(defun copy-string (orig)
  (let ((new (make-string (length orig))))
    (loop for ix from 0
       for ch across orig do
	 (setf (aref new ix) ch))
    new))

(defgeneric copy (object))

(defmethod copy (object)
  object)

(defmethod copy ((object string))
  (copy-string object))

(defun copy-improper-list (list)
  (cond ((null list)
	 list)
	((atom list)	     
	 (copy list))
	(t (cons (copy (car list))
		 (copy-improper-list (cdr list))))))
	

(defmethod copy ((object list))
  (copy-improper-list object))

(defun join (strings delim)
  (apply 'concatenate (cons 'string
			    (loop for (first rest) on strings
			       collect first
				 when rest collect delim))))

(defun begins-with-p (prefix sequence &key (test #'eql))
  (let ((prefix-list (coerce prefix 'list))
	(sequence-list (coerce sequence 'list)))
    (and (>= (length sequence)
	     (length prefix))
	 (loop for p in prefix-list
	    for s in sequence-list
	      always (funcall test p s)))))

(defun split (string delim)
  (setf string (coerce string 'list))
  (let ((result nil)
	(current-word nil))
    (loop while string
       do (cond ((begins-with-p delim string)
		 (push (coerce (reverse current-word) 'string) result)
		 (setf current-word nil)
		 (loop repeat (length delim) do (pop string)))
		(t (push (pop string) current-word))))
    (reverse (if current-word
		 (cons (coerce (reverse current-word) 'string) result)
		 result))))

(defun raise-simple-error (type format &rest args)
  (error type :format-control format :format-arguments args))

(defgeneric format-error (err))
(defmethod format-error ((err simple-error))
  (format-simple-error err))
(defmethod format-error ((err sb-int:simple-parse-error))
  (format-simple-error err))
(defmethod format-error ((err simple-condition))
  (format-simple-error err))


(defun format-simple-error (err)
  (apply 'format nil (slot-value err 'sb-kernel::format-control)
	 (slot-value err 'sb-kernel::format-arguments)))

(defun insert-into-list (list new-elem &key before after)
  (cond ((and before after)
	 (error "Can't INSERT-INTO-LIST using both :BEFORE and :AFTER"))
	((not (or before after))
	 (error "INSERT-INTO-LIST requires either :BEFORE or :AFTER to be provided."))
	(t (loop for old-elems on list
	      for ix from 1
	      when (and before (= ix before))
	        return (append head (list new-elem) old-elems)
	      collect (car old-elems) into head
	      when (and after (= ix after))
		return (append head (list new-elem) (cdr old-elems))))))

(defun insert-before! (new-elem before list &key (test #'eql))
  (loop for existing-elems on list
     for (first . rest) = existing-elems
     when (funcall test first before)
     do (setf (car existing-elems) new-elem)
       (setf (cdr existing-elems)
	     (cons first (cdr existing-elems)))
       (return list)))

(defun remove-at! (n elem list &key (test #'eql))
  (loop for existing-elems on list
     when (funcall test (car existing-elems) elem)
     do (loop repeat n
	   do (setf (car existing-elems)
		    (car (cdr existing-elems)))
	     (setf (cdr existing-elems)
		   (cddr existing-elems)))
       (return list)))

(defmacro stringcase (form &body cases)
  (let ((symb (gensym)))
    `(let ((,symb ,form))
       (declare (ignorable ,symb))
       (cond ,@(loop for (strings . body) in cases
		  collect (cons (cond ((listp strings)
				       `(member ,symb ',strings :test #'string=))				      
				      ((and (symbolp strings)
					    (string= (symbol-name strings) "OTHERWISE"))
				       t)
				      (t `(equalp ,strings ,symb)))
				body))))))

(defmacro estringcase (form &body cases)
  (if (loop for (strings . body) in cases
	 thereis (and (symbolp strings)
		      (string= (symbol-name strings) "OTHERWISE")))
      `(stringcase form ,@cases)
      (let ((value (gensym)))
	`(let ((,value ,form))
	   (declare (ignorable ,value))
	   (stringcase ,value ,@cases
		       (otherwise (error "~S fell through ESTRINGCASE expression. Wanted one of ~S" ,value ',(apply #'append
														    (mapcar #'car cases)))))))))

(defun group (list &key (test #'eql) (key #'identity))
  (let ((groups nil)
	(current-group nil))
    (loop for (first . rest) on list
       do (push first current-group)
	 (unless (and rest
		      (funcall test (funcall key first)
			       (funcall key (car rest))))
	   (push (reverse current-group) groups)
	   (setf current-group nil)))
    (reverse groups)))

(defgeneric take (n sequence))
(defgeneric take-while (pred sequence))
(defgeneric drop (n sequence))
(defgeneric drop-while (pred sequence))

(defmethod take (n (sequence list))
  (loop for item in sequence repeat n collect item))

(defmethod take (n (sequence vector))
  (coerce 
   (loop for item across sequence repeat n collect item)
   'vector))

(defmethod take (n (sequence string))
  (coerce 
   (loop for item across sequence repeat n collect item)
   'string))  

(defmethod take-while (pred (sequence list))
  (loop for item in sequence while (funcall pred item)
       collect item))

(defmethod drop (n (sequence list))
  (loop repeat n do (pop sequence))
  sequence)

(defmethod drop-while (pred (sequence list))
  (loop while (and sequence
		   (funcall pred (car sequence)))
     do (pop sequence))
  sequence)

(defgeneric split-when (pred sequence))
(defmethod split-when (pred (sequence list))
  (loop for (hd . tl) on sequence
     until (funcall pred hd) collect hd into before-split
       finally (return (values before-split (cons hd tl)))))


(defun recursive-find/collect (pred tree)
  (loop for item in tree
     if (funcall pred item) collect item
       else if (listp item) append (recursive-find/collect pred item)))

(defun recursive-find (pred tree)
  (loop for item in tree
     if (funcall pred item) return item
     else do (if (listp item)
		 (aif (recursive-find pred item)
		      (return it)))))

(defun recursive-mapcar (function tree &optional traverse-results result)
  "Maps the FUNCTION onto every node in the TREE. Works correctly with
improper lists.

If TRAVERSE-RESULTS is non-NIL, then RECURSIVE-MAPCAR will traverse
the result of the FUNCTION even if it is not EQ to the original value.

"
  (let ((new-tree (funcall function tree)))
    (when traverse-results
      (setf tree new-tree))
    (cond ((not (eq new-tree tree))
	   (append (reverse result) new-tree))
	  ((null tree)
	   (reverse result))
	  ((atom tree)
	   (if result
	       (append (reverse result) tree)
	       tree))
	  ((consp tree)
	   (recursive-mapcar function (cdr tree)
		     traverse-results
		     (cons (recursive-mapcar function (car tree)
					     traverse-results) result))))))

(defun map-leaves (function tree)
  "Maps the FUNCTION across only the leaf nodes of the TREE."
  (mapcar (lambda (node)
	    (if (listp node)
		(map-leaves function node)
		(funcall function node))) tree))

(defun recursive-find/replace (what with tree &key (test #'eql) (key #'identity))
  "Replaces all instances of WHAT with WITH in all nodes of the TREE."
  (recursive-mapcar
   (λ (node)
     (if (funcall test what (funcall key node))
	 with
	 node))
   tree))

(defmacro extend-list (new-value list last-tail)
  "Builds a list in the forward direction. LIST and LAST-TAIL must be
SETFable places."
  `(progn
     (cond (,list
	    (rplacd ,last-tail (cons ,new-value nil))
	    (setf ,last-tail (cdr ,last-tail)))
	   (t
	    (setf ,list (list ,new-value))
	    (setf ,last-tail ,list)))
     ,list))


(defun sift (pred list)
  (loop for item in list
     when (funcall pred item)
     collect item into gold
     else collect item into dirt
       finally (return (values gold dirt))))

(defmacro setf-if-nil (place value)
  `(unless ,place
     (setf ,place ,value)))

(defun positivep (n)
  (> n 0))

(defun negativep (n)
  (< n 0))

(defun pathname-append (first &rest paths)
  (let ((result first))
    (loop for p in paths do
	 (setf result (merge-pathnames p result)))
    result))

(defgeneric to-string (obj))

(defmethod to-string ((obj pathname))
  (format nil "~a" obj))

(defmethod to-string ((obj string))
  obj)

(defmethod to-string ((obj symbol))
  (symbol-name obj))

(defmethod to-string ((obj character))
  (coerce (list obj) 'string))

(defmethod to-string ((obj number))
  (format nil "~a" obj))

(defmethod to-string (obj)
  (format nil "~a" obj))

(defmacro destructuring-lambda (pattern &body body)
  (let ((arg (gensym)))
    `(lambda (,arg)
       (destructuring-bind ,pattern ,arg ,@body))))

(defun char-range (low high)
  (loop for n from (char-code low) to (char-code high)
       collect (code-char n)))

(defmacro let-values* (bindings &body body)
  "Similar to let-values from Racket, except the bindings are sequential."
  (let ((result `(progn ,@body)))
    (loop for (variables value-form) in (reverse bindings)
       do (setf result (if (listp variables)
			   `(multiple-value-bind ,variables ,value-form
			      ,result)
			   `(let ((,variables ,value-form))
			      ,result))))
    result))

(defun set-intersection (l1 l2)
  (loop for item in l1
     when (member item l2)
       collect item))

(defun print-signs (location-range-string)
  (loop for (low high) in (locations:parse-locations location-range-string)
     do (locations:do-location-range (low high) loc
	  (sb-ext:run-program "print-one-sign" (list loc) :search t))))

(defmacro foreach-sequence (var sequence &body body)
  (with-gensyms (seq-var)
    `(let ((,seq-var ,sequence))
       (etypecase ,seq-var
	 (list
	  (loop for ,var in ,seq-var do (progn ,@body)))
	 ((or vector string simple-array)
	  (loop for ,var across ,seq-var do (progn ,@body)))))))

(defmacro foreach (variables value-expression &body body)
  "Iterates over the sequence generated by VALUE-EXPRESSION. If VALUE-EXPRESSION produces more
than one value, then VARIABLES can be a list denoting multiple variables to bind to. Iteration
occurs only over the first value. If VALUE-EXPRESSION produces only one value, then VARIABLES
can be a symbol specifying a single value."
  (if (symbolp variables)
      `(foreach-sequence ,variables ,value-expression ,@body)
      (let ((list (gensym)))
	`(multiple-value-bind (,list ,@(cdr variables)) ,value-expression
	   (foreach-sequence ,(car variables) ,list ,@body)))))

(defmacro named-let (name bindings &body body)
  "Like the 'named let' form from Scheme. NOTE: You can't name the LET block 'loop' like
you would in Scheme, because that name is taken by the Common Lisp LOOP macro."
  `(labels ((,name ,(mapcar #'first bindings)
	      ,@body))
     (,name ,@(mapcar #'second bindings))))

(defun fast-remove-duplicates (sortable-list &key (sort-comparator #'<) (test #'eql) (key #'identity))
  "Like REMOVE-DUPLICATES, except optimized for sortable objects."
  (loop for (current . next) on (sort sortable-list sort-comparator :key key)
     unless (and (not (null next))
		 (funcall test (funcall key current) (funcall key (car next))))
       collect current))

(defun mapcar/append (func &rest lists)
  (apply #'append
	 (apply #'mapcar (cons func lists))))

(defun explode (string)
  "Like PHP's eponymous function, converts the STRING into a list of strings, each one containing
only one character from the original string."
  (mapcar (hof:compose (hof:curry (hof:flip-args 'coerce) 'string)
		       #'list)
	  (coerce string 'list)))

(defgeneric last-element (sequence))
(defmethod last-element ((sequence list))
  (car (last sequence)))
(defmethod last-element ((sequence vector))
  (if (= (length sequence) 0)
      nil
      (aref sequence (- (length sequence) 1))))

(defmacro with-retry (restart-name restart-description &body body)
  (let ((tag-name (gensym))
	(block-name (gensym)))
    `(block ,block-name
       (tagbody
	  ,tag-name
	  (restart-case
	      (return-from ,block-name
		(progn ,@body))
	    (,restart-name () :report ,restart-description
			   (go ,tag-name)))))))

(defun get-restart (name)
  (loop for restart in (compute-restarts)
     when (equalp (to-string (restart-name restart))
		  (to-string name))
     do (return-from get-restart restart))
  (error "No restart with name matching ~a" name))

(defun invoke-restart-by-name (name)
  "Like INVOKE-RESTART, except it doesn't matter what package NAME is in."
  (aif (get-restart name)
       (invoke-restart it)
       (error "No restart named ~a" name)))

(defun zero-pad (len string)
  (declare (type fixnum len)
	   (type string string))
  (named-let local-loop
      ((result (coerce string 'list)))
    (if (>= (length result) len)
	(coerce result 'string)
	(local-loop (cons #\0 result)))))

(defmacro λ (lambda-list &body body)
  `(lambda ,lambda-list ,@body))

(defun file->string (pathname &optional (external-format :utf-8))
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let* ((length (file-length in))
	   (result (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence result in)
      (flexi-streams:octets-to-string result :external-format external-format))))
	  
(defun integer->boolean (integer)
  "Converts a C-style boolean to a Lisp-style boolean."
  (unless (not integer)
    (not (eq integer 0))))

(defvar *sort-by-eq* #'eq)

(defun sort-by (list comparator &rest keys)
  "Sorts a list by multiple keys (accessors), in the precedence defined
by the order the keys are given. Comparison stops when two keys are non-EQ.
The equality operator can be overridden by setting UTILS:*SORT-BY-EQ* to
a different function."
  (setf keys
	(loop for k in keys
	   collect (etypecase k
		     (string (curry #'gethash k))
		     (function k))))
  (sort list
	(λ (a b)
	  (loop for key in keys
	     for key-a = (funcall key a)
	     for key-b = (funcall key b)
	     if (funcall comparator key-a key-b)
	       return it
	     else unless (funcall *sort-by-eq* key-a key-b)
	       return nil))))

(defun sql-timestamp->integer (timestamp)
  "Converts a PostgreSQL timestamp into an integer representing a number of seconds, suitable
for using as a sort-key here in the server-app, or on the browser.
Drops the microsecond-level information that PG supplies because the library used to do the
actual parsing doesn't support that."
   (parse-time (cl-ppcre:regex-replace-all "\\..*$" timestamp "")))

(defun string-or-null (string)
  (if (string= string "")
      nil
      string))

(defmacro map-values (function value-form)
  `(apply #'values (mapcar ,function (multiple-value-list ,value-form))))

(defmacro apply-values (function value-form)
  `(apply #'values (apply ,function (multiple-value-list ,value-form))))

(defmacro reverse-values (value-form)
  `(apply #'values
	  (reverse (multiple-value-list ,value-form))))

(defgeneric split-at (n sequence))
(defmethod split-at (n (sequence list))
  (values (loop repeat n while sequence collect (pop sequence))
	  sequence))

(defmethod split-at (n (sequence string))
  (map-values (curry (flip-args #'coerce) 'string)
   (split-at n (coerce sequence 'list))))

(defmethod split-at (n (sequence vector))
  (map-values (curry (flip-args #'coerce) 'vector)
   (split-at n (coerce sequence 'list))))

(defun select-columns (hashes &rest columns)
  (loop for hash in hashes collect
       (let ((result (make-hash-table :test 'equalp)))
	 (maphash (λ (k v)
		    (if (member k columns :test #'equalp)
			(setf (gethash k result) v)))
		  hash)
	 result)))

(defmacro string+= (place &rest strings)
  `(setf ,place (concatenate 'string
			     ,place ,@strings)))

(defun expand-typecheck (typep var)
  (cond ((and (consp typep)
	      (eq (car typep) 'or))
	 `(or
	   ,@(loop for actual-typep in (cdr typep)
		collect `(,actual-typep ,var))))
	((and (consp typep)
	      (eq (car typep) 'not))
	 `(not ,(expand-typecheck (cadr typep) var)))
	(t `(,typep ,var))))

(defmacro debuggable-typecheck (&rest typespecs)
  `(progn
     ,@(loop for (typep . variables) in typespecs
	  collect `(progn ,@(loop for v in variables
			       collect `(unless ,(expand-typecheck typep v)
					  (error "Variable ~S with value ~S and type ~S does not satisfy ~S"
						 ',v ,v (type-of ,v) ',typep)))))))

(defmacro simple-pipe (form-1 form-2)
  (with-gensyms (form-output)
    `(let ((,form-output (with-output-to-string (*standard-output*)
			   ,form-1)))
       (with-input-from-string (*standard-input* ,form-output)
	 ,form-2))))

(defmacro pipe (&body forms)
  "Like the Unix shell pipe operator, except for Lisp forms. The *standard-output* of each form
becomes the *standard-input* of the next form."
  (if (null forms)
      nil
      (let ((first-form (car forms))
	    (rest-forms (cdr forms)))
	`(simple-pipe ,first-form
		      ,(if (= (length rest-forms) 1)
			   (car rest-forms)
			   `(pipe ,@rest-forms))))))

(defun grep (regexp &key v i)
  "Like Unix GREP."
  (if i
      (setf regexp (string-downcase regexp)))
  (loop for line = (read-line *standard-input* nil 'eof)
       until (eq line 'eof)
     for matchedp = (cl-ppcre:all-matches regexp (if i
						     (string-downcase line)
						     line))
     when (if v
	      (not matchedp)
	      matchedp)
       do (write-line line)))
       
