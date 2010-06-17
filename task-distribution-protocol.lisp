(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.task-distribution-protocol
  (:nicknames :ws.protocol)
  (:use :common-lisp
	:common-lisp-user
	:ws.g)
  (:export :*default-master-port*
	   :*default-slave-port*
	   :single-task-p
	   :resource-p
	   :send-task
	   :send-resource))

(in-package :ws.protocol)





(defstruct connection
  (establishment-time nil)
  (address nil))



;; utility functions
;;------------------

;; ok
(defun read-file-contents (filename)
  "Reads whole file and returns list of sexps. Works only for lisp files."
  (with-open-file (file filename)
    (let ((file-contents nil))
      (handler-case
	  (do nil (nil nil)	;; infinite loop till EOF exception
	    (setf file-contents ;; reading contents to the variable
		  (if (null file-contents)
		      (read file)
		      (list file-contents
			    (read file)))))
	(end-of-file () file-contents)))))



;; ok
(defun send-printable-object (socket obj)
  (let ((*package* (find-package :ws.protocol)))
    (let ((string-to-send (concatenate 'string
				       (prin1-to-string obj)
				       (list #\return #\newline))))
      (sb-bsd-sockets:socket-send socket string-to-send
				  (length string-to-send)))))



;; OK
;; Check the usage of CONNECTION-VAR. Should it be evaluated before ADDRESS and PORT?
;; Should there be any protection against the "(with-tcp-connection (gensym) ...)" case?
;; What if connection-var will hold a function with side-effects? Will it be the case of
;; less astonishment for user?
(defmacro with-tcp-connection (connection-var address port &body body)
  (let ((address-param (gensym))
	(port-param (gensym)))
    `(let ((,address-param ,address)
	   (,port-param ,port)
	   (,connection-var nil))
       (unwind-protect
	    (progn
	      (setf ,connection-var (make-instance 'sb-bsd-sockets:inet-socket
						   :type :stream
						   :protocol :tcp))
	      (sb-bsd-sockets:socket-connect ,connection-var
					     ,address-param
					     ,port-param)
	      ,@body)
	 (sb-bsd-sockets:socket-close ,connection-var)))))





;; protocol functions
;;-------------------



;; fix, get rid of with-tcp-connection, throw an exception in case of fail, not only return nil
(defun send-task (task task-id name respond-to slave)
  "Low-level function that sends lisp objects to a remote host. In case of errors during execution REMOTE-ERROR exception is raised and nil is returned. If ok, t is returned."
  (with-tcp-connection connection (end-point-ip slave) (end-point-port slave)
		       (send-printable-object connection `(TASK (ID-IP ,(end-point-ip (task-id-end-point task-id)))
								(ID-PORT ,(end-point-port (task-id-end-point task-id)))
								(ID-NUMBER ,(task-id-number task-id))
								(NAME ,name)
								(RESPOND-TO-IP ,(end-point-ip respond-to))
								(RESPOND-TO-PORT ,(end-point-port respond-to))
								,task))
		       (let ((socket-stream (sb-bsd-sockets:socket-make-stream connection
									       :input t
									       :output t))
			     (slave-answer nil))
			 (handler-case
			     (setf slave-answer (read socket-stream))
			   (END-OF-FILE () (progn
					     (setf slave-answer 'nil))))
			 (format t "slave said:~a~%" slave-answer) ;; debug
			 (cond
			   ((equal (symbol-name slave-answer) "TASK-OK")
			    t)
			   ((equal (symbol-name slave-answer) "BAD-FORMAT")
			    (error "Slave ~a:~a did not understand the message."
				   (end-point-ip slave)
				   (end-point-port slave))
			    nil)
			   ((null slave-answer)
			    (error "Connection to slave ~a:~a lost while sending task~%"
				   (end-point-ip slave)
				   (end-point-port slave)))
			   (t (error "Slave answered in an unknown manner:~a"
				     slave-answer)
			      nil)))))



;; ok
(defun single-task-p (task)
  (and (listp task)
       (equal (symbol-name (first task)) "TASK")
       (equal (symbol-name (first (second task))) "ID-IP")
       (equal (symbol-name (first (third task))) "ID-PORT")
       (equal (symbol-name (first (fourth task))) "ID-NUMBER")
       (equal (symbol-name (first (fifth task))) "NAME")
       (equal (symbol-name (first (sixth task))) "RESPOND-TO-IP")
       (equal (symbol-name (first (seventh task))) "RESPOND-TO-PORT")))
;;       (= (length (second task)) 2)
;;       (= (length (third task)) 2)
;;       (= (length (sixths task)) 3)
;;       (eql (first (type-of (second (fourth task)))) 'SIMPLE-VECTOR)
;;       (= (second (type-of (second (fourth task)))) 4))) ;; ensure that vector has 4 elements



;; mock
(defun resource-p (list)
  (and (listp list)
       (equal (symbol-name (first list)) "RESOURCE")))



;; ok
;;(defun send-protocol-error (socket error-name)
;;  (send-printable-object socket error-name))



;; mock
(defun send-binary-file (file-name socket)
  (let ((bulk (make-array 100000 :element-type '(unsigned-byte 8) :initial-element 0))
	(file-length 0))
    (with-open-file (file-stream file-name :element-type '(unsigned-byte 8))
      (setf file-length (file-length file-stream))
      (format t "file opened, length:~a~%" file-length)
      (handler-case
	  (progn
	    (do ((bytes-read (read-sequence bulk file-stream)
			     (read-sequence bulk file-stream))
		 (total 0))
		((= bytes-read 0)) ;; end-case
	      (setf total (+ total bytes-read))
	      (format t "read-sequence returned:~a, total:~a~%" bytes-read total)
	      (format t "(send) returned:~a~%"
		      (sb-bsd-sockets:socket-send socket bulk bytes-read)))
	    (format t "all data sent. exiting send-binary-file...~%"))
	(END-OF-FILE ()
	  (format t "eof~%"))))))



;; mock
(defun get-file-size (file-name)
  (with-open-file (file-stream file-name :element-type '(unsigned-byte 8))
    ;; warning. if :element-type is not (unsigned-byte 8) then the function
    ;; file-length may return unpredictable result (implementation-dependent)
    (file-length file-stream)))
    



;; mock
(defun send-resource (file-name resource-name slave)
  (with-tcp-connection connection (end-point-ip slave) (end-point-port slave)
    (send-printable-object connection `(RESOURCE
					,resource-name
					,(get-file-size resource-name)))
    (let* ((socket-stream (sb-bsd-sockets:socket-make-stream connection
							     :input t
							     :output t))
	   (slave-answer nil)
	   (bytes-slave-received 0))
      (format t "connecting to port ~a~%" (end-point-port slave))
      (format t "sending file:~a~%" file-name)
      (format t "reading slave's answer...~%")
      (setf slave-answer (read socket-stream))
      (format t "slave said:~a~%" slave-answer)
      (cond
	((equal (symbol-name slave-answer) "RESOURCE-OK")
	 (send-binary-file file-name connection)
	 (setf bytes-slave-received (read socket-stream))
	 (format t "bytes slave received:~a~%" bytes-slave-received)
	 (sb-bsd-sockets:socket-close connection))
        ((equal (symbol-name slave-answer) "BAD-FORMAT")
	 (error "Slave ~a:~a did not understand the message."
		(end-point-ip slave)
		(end-point-port slave)))
	 (t (error "Slave answered in an unknown manner:~a"
		   slave-answer))))))
