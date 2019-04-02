(defpackage :actors
  (:use :common-lisp :utils :ipc :handler-case*)
  (:export :actor :send :stop :defactor :channel-send :channel-receive :self :server :*current-channel*))

(in-package :actors)

(defclass actor ()
  ((mailbox :initform (make-mailbox) :initarg :mailbox)
   (logic :type sb-thread:thread :initarg :logic)))

(defmacro actor (lambda-list &rest body)
  "Similar to LAMBDA, except the object created is an ACTOR instead of a FUNCTION. The ACTOR will wait
for messages and evaluate the BODY sequentially each time a message is received.

You can send messages to the actor with SEND. The &REST arguments to SEND must match the actor's lambda-list
with one exception: A message consisting of the single argument 'ACTORS:STOP will terminate the actor's thread
immediately without attempting to bind the actor's arguments or evaluate its BODY. However, the 'ACTORS:STOP
message cannot interrupt the BODY if it is being evaluated when the message is sent. 'ACTORS:STOP is processed
when the actor begins waiting for a new message.

Within BODY, the variable ACTORS:SELF is lexically bound to
the current actor."
  `(let* ((self nil)
	  (my-lambda (lambda ,lambda-list ,@body))
	  (my-mailbox (make-mailbox)))     
     (setf self (make-instance 'actor :mailbox my-mailbox
			       :logic (sb-thread:make-thread
				       (lambda ()
					 (loop for message = (get-message my-mailbox)
					    until (equalp message '(stop))
					    do (restart-case
						   (apply my-lambda message)
						 (skip-message () :report "Abort processing of this message"
							       nil))))
				       :name "Actor")))))

(defmacro defactor (name lambda-list &body body)
  "Like DEFUN but for actors. The resulting NAME is a variable whose
value is an actor."
  `(progn (defparameter ,name (actor ,lambda-list ,@body))
	  (defun ,name (&rest args)
	    (apply 'send (cons ,name args)))))
	    

(defgeneric send (actor &rest message))

(defmethod send ((actor actor) &rest message)
  (send-message (slot-value actor 'mailbox) message))

(defclass two-way-channel ()
  ((inbound :initarg :inbound :initform (make-mailbox))
   (outbound :initarg :outbound :initform  (make-mailbox))))

(defvar *channel-role* :client)
(defvar *default-channel-timeout* (* 120 60))
(defvar *current-channel* nil)

(defun channel-send (channel message &key (role *channel-role*))
  "Sends a message through a two-way-channel. The actors on either end of a two-way-channel
play two roles on this channel: Either :CLIENT or :SERVER."
  (send-message (ecase role
		  ((:server) (slot-value channel 'outbound))
		  ((:client) (slot-value channel 'inbound))) message))

(defun channel-receive (channel &key (timeout *default-channel-timeout*) (role *channel-role*) error-if-empty default-value non-blocking)
  (get-message (ecase role
		 ((:server) (slot-value channel 'inbound))
		 ((:client) (slot-value channel 'outbound)))
	       :error-if-empty error-if-empty
	       :default-value default-value
	       :timeout timeout
	       :non-blocking non-blocking))

(defmacro server (var &rest body)
  "Spawns a thread that you can interact with through a TWO-WAY-CHANNEL.
In the new thread, that channel will be bound to the VAR (and also to ACTORS:*CURRENT-CHANNEL*) and then the BODY
will be evaluated. The thread terminates when the BODY completes.

The SERVER form returns two values: The two-way-channel through which you may communicate
interactively with the server, and the thread object."

  `(let ((actor (actor (,var)
		       (declare (type two-way-channel ,var))
		       (send self 'stop)
		       (let ((*channel-role* :server)
			     (*current-channel* ,var))
			 (assert (eq *channel-role* :server))
			 ,@body)))
	 (channel (make-instance 'two-way-channel)))
     (send actor channel)
     (values channel (slot-value actor 'logic))))

(defmacro with-interaction-exception-handler (&body body)
  `(handler-case*
    (progn ,@body)
    ((or errors:application-error parse-error)
     (exn) :before-unwind
     (progn
       (channel-send *current-channel*
		     `(div
		       ,(if (eq (type-of exn) 'sb-int:simple-parse-error)
			    `(p "Invalid integer")
			    `(p "An error occurred."))
		       (p ,(format-error exn))
		       (p ,(format nil "(Error of type ~a)" (type-of exn)))
		       (p "Restarts: ")
		       (ul
			,@(loop for restart in (compute-restarts)
			     collect `(li (div ,(with-output-to-string (out)
						  (sb-kernel::restart-report restart out)))
					  (div "(" ,(restart-name restart) ")"))))))
       (invoke-restart (get-restart (channel-receive *current-channel*)))))))
		    
(defun server-interaction-repl (channel &key receive-first)
  (when receive-first
    (print (channel-receive channel :timeout 3)))
  (loop for command = (progn
			(format t "~%REPL> ")
			(finish-output)
			(eval (read)))
     do (channel-send channel command)
       (print (channel-receive channel :timeout 3))))
