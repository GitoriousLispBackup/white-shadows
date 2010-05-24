;; This file contains Task Distribution Protocol (TDP) realization.
;; This protocol is a set of rules and actions order two hosts
;; (usually master and slave) must obey.

;; TDP is a protocol for task interchange in a distributed system.
;; All hosts are divided into masters and slaves. One host may be
;; master and slave at the same time. Master creates and distributes
;; tasks, slaves executes them and send results back to master.

;; 1. Master connects to a slave:
;; (let ((connection (ws.protocol:establish '("127.0.0.1"))))
;;   ...

;; 2. Master sends a single task to a slave:
;; (let ((connection (ws.protocol:establish '("127.0.0.1"))))
;;   (ws.protocol:send-single-task connection '(+ 2 3))
;;   ...

;; This was an asynchronous call, send-single-task doesn't wait for
;; form execution it just raises an exception in case of protocol
;; errors or returns nil if everything is ok.

;; 3. Master waits for results of sent task execution
;; (let ((connection (ws.protocol:establish '("127.0.0.1"))))
;;   (ws.protocol:send-single-task connection '(+ 2 3))
;;   (ws.protocol:with-response connection
;;     (do something 1)
;;     (do something 2)))

;; 3. Master closes a connection
;; (with-tcp-connection connection '("127.0.0.1")
;;   (let ((response-socket (make-response-socket))
;;         (respond-to ws.p-table:bind-socket-to-free-port response-socket))
;;     (ws.protocol:send-single-task connection :id 234 :name "lol-task" :respond-to listener :task '(+ 2 3))
;;     (ws.protocol:close-connection connection)))

;; Or a safe form should be used:
;; (ws.protocol:with-connection connection '("127.0.0.1")
;;   (ws.protocol:send-single-task connection '(+ 2 3)))


;; protocol documentation
;; 1. send a single task:
;;   A. Master establishes tcp connection to a slave
;;   B. Master sends a text message of the following format:
;;
;;   (SINGLE-TASK (ID <a number>) (NAME <a string> ) (RESPOND-TO <a vector> <a number>)
;;     (<TASK CODE1>)
;;     (<TASK CODE2>))
;;
;;     A vector and a number in the (RESPOND-TO ..) section are ip-address and a port
;;     <TASK CODE> are forms that are to be executed
;;
;;   C. Slave may respond with the following messages:
;;      (OK) - task approved, but it may begin it's execution in a future
;;      (BAD-FORMAT) - task is not appropriately formed
;;
;;   D. After slave responds, tcp connection is closed.



(in-package :common-lisp-user)

(defpackage h2s04.white-shadow.task-distribution-protocol
  (:nicknames :ws.protocol)
  (:use :common-lisp
	:common-lisp-user
	:sb-bsd-sockets))

(in-package :ws.protocol)



(defparameter *default-port* 30033 "Default slave's listening port")



(defstruct connection
  (establishment-time nil)
  (address nil))

(defstruct node
  (ip nil)
  (port nil))

(defstruct address
  (ip 0)
  (port *default-port*))



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
  (let ((obj-str (concatenate 'string
			      (prin1-to-string obj)
			      (list #\return #\newline))))
    (sb-bsd-sockets:socket-send socket obj-str (length obj-str))))



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

(defun test ()
  (with-tcp-connection cnv (vector 127 0 0 1) 2993
		       (sb-bsd-sockets:socket-send cnv "far far away - --===000-- -" 18)))


;; for testing purposes
(defun make-print-server ()
  (sb-thread:make-thread (lambda ()
			   (let ((buffer (make-array 250 :element-type 'character :initial-element #\a))
				 (socket (make-instance 'sb-bsd-sockets:inet-socket
							:type :stream
							:protocol :tcp)))
			     (unwind-protect
				  (progn
				    (sb-bsd-sockets:socket-bind socket (vector 127 0 0 1) 2993)
				    (sb-bsd-sockets:socket-listen socket 5)
				    (format t "listen over. acceptig...~%")
				    (let ((client-socket (socket-accept socket)))
				      (format t "accepted. receiving...~%")
				      (sb-bsd-sockets:socket-receive client-socket buffer nil)
				      (format t "received data:~a~%" buffer)
				      (format t "closing socket normally...")
				      (sb-bsd-sockets:socket-close client-socket)))
			       (progn (sb-bsd-sockets:socket-close socket)
				      (print "unwind-protect:socket closed")))))))


	      


;; protocol functions
;;-------------------

;; ok
(defun send-task (task id name respond-to-ip respond-to-port slave)
  "Low-level function that sends lisp objects to a remote host. In case of errors during execution REMOTE-ERROR exception is raised."
  (with-tcp-connection connection (node-ip slave) (node-port slave)
    (send-printable-object connection `(TASK (ID ,id)
						    (NAME ,name)
						    (RESPOND-TO ,respond-to-ip ,respond-to-port)
						    ,task))))



;; ok
(defun single-task-p (task)
  (and (listp task)
       (eql (first task) 'TASK)))

;; ok
(defun validate-single-task (task)
    (and (eql (first task) 'TASK)
         (eql (first (second task)) 'ID)
         (eql (first (third task)) 'NAME)
         (eql (first (fourth task)) 'RESPOND-TO)
         (= (length (second task)) 2)
         (= (length (third task)) 2)
         (= (length (fourth task)) 3)
         (eql (type-of (second (fourth task))) 'SIMPLE-VECTOR)
         (= (second (type-of (second (fourth task)))) 4)))

(defun execute-task (task)
  ())
;; mock
;;(defun send-file (filename socket) ;; sockets??? streams??? read node.lisp
;;  (let ((file-contents (read-file-contents filename)))
;;    (send-printable-object socket file-contents)
;;    (let ((response (read socket

;; mock
;;(defun send-files (files-list socket)
;;  "Low-level function that sends bunch of files to a remote host. FILES-LIST contains file paths, e.g. '(/home/user/file1.lisp /home/user/file2.lisp). Each file is sent and executed in order of appearence in the FILES-LIST. Files are executed only after they all are received. In case of error during execution REMOTE-ERROR exception is raised."
;;  (dolist (filename files-list)
;;    (let ((file-contents (read-file-contents filename)))
;;      (send-printable-object socket (list