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
;;   B. Master sends a five-digit string that represents the length of a folowing message. If this number is less than five digits long then it must be prepended with zeros: 00024 or 01234
;;   C. Master sends a text message of the following format:
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

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.task-distribution-protocol
  (:nicknames :ws.protocol)
  (:use :common-lisp
	:common-lisp-user)
  (:export :*default-slave-port*
	   :single-task-p
	   :send-task
	   :send-protocol-error))

(in-package :ws.protocol)



(defparameter *default-slave-port* 30033 "Default slave's listening port")



(defstruct connection
  (establishment-time nil)
  (address nil))

(defstruct node
  (ip nil)
  (port nil))

(defstruct address
  (ip 0)
  (port *default-slave-port*))



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
  (send-task '(+ 2 3 "gg") 234 "unnamed" (vector 127 0 0 1) 4040 (make-node :ip (vector 127 0 0 1)
									    :port 2986)))





;; protocol functions
;;-------------------



;; ok
(defun send-task (task id name respond-to-ip respond-to-port slave)
  "Low-level function that sends lisp objects to a remote host. In case of errors during execution REMOTE-ERROR exception is raised."
  (with-tcp-connection connection (node-ip slave) (node-port slave)
    (send-printable-object connection `(TASK (ID ,id)
					     (NAME ,name)
					     (RESPOND-TO ,respond-to-ip ,respond-to-port)
					     ,task))
    (let* ((socket-stream (sb-bsd-sockets:socket-make-stream connection
							     :input t
							     :output t))
	   (slave-answer (read socket-stream)))
      (format t "slave said:~a~%" slave-answer) ;; debug
      (cond
	((equal (symbol-name slave-answer) "TASK-OK")
	 nil)
	((equal (symbol-name slave-answer) "BAD-FORMAT")
	 (error "Slave ~a:~a did not understand the message."
		(node-ip slave)
		(node-port slave)))
	(t (error "Slave answered in an unknown manner:~a"
		  slave-answer))))))



;; ok
(defun single-task-p (task)
  (and (listp task)
       (equal (symbol-name (first task)) "TASK")
       (equal (symbol-name (first (second task))) "ID")
       (equal (symbol-name (first (third task))) "NAME")
       (equal (symbol-name (first (fourth task))) "RESPOND-TO")
       (= (length (second task)) 2)
       (= (length (third task)) 2)
       (= (length (fourth task)) 3)
       (eql (first (type-of (second (fourth task)))) 'SIMPLE-VECTOR)
       (= (second (type-of (second (fourth task)))) 4))) ;; ensure that vector has 4 elements



;; ok
(defun send-protocol-error (socket error-name)
  (send-printable-object socket error-name))



;; mock
(defun send-resource (file-name resource-name slave)
  (with-tcp-connection connection (node-ip slave) (node-port slave)
    (send-printable-object connection `(RESOURCE ,resource-name))
    (let* ((socket-stream (sb-bsd-sockets:socket-make-stream connection
							     :input t
							     :output t))
	   (slave-answer (read socket-stream)))
      (format t "slave said:~a~%" slave-answer)
      (cond
	((equal (symbol-name slave-answer) "RESOURCE-OK")
	 (send-binary-file file-name connection)
	 ;; finish this func)))))



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