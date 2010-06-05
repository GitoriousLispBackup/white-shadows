;; depends on: ws.protocol, ws.network, ws.config

(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.master
  (:nicknames :ws.master)
  (:use :common-lisp
	:common-lisp-user
	:ws.g)
  (:export :start-slave-accepter
	   :stop-slave-accepter))

(in-package :ws.master)





(defstruct node
  (cpu-count 0)
  (system-load-value 0.1) ;; floating-point value, obtained by dividing cpu-count by number of active threads
  (mem-total 0)
  (mem-free 0)
  (architecture "I686")
  (ip nil)
  (port nil)
  (append-time 0))



(defun find-item (item slave-tests)
  (find item slave-tests :test #'(lambda (item-to-search candidate)
				   (equal
				    (symbol-name (first candidate))
				    item-to-search))))



(defun make-node-from-scratch (ip append-time tests)
  (make-node :ip ip
	     :port (second (find-item "LISTEN-PORT" tests))
	     :cpu-count (second (find-item "CPUS" tests))
	     :mem-total (second (find-item "MEM-TOTAL" tests))
	     :mem-free (second (find-item "MEM-FREE" tests))
	     :architecture (symbol-name (second (find-item "ARCHITECTURE" tests)))
	     :append-time append-time))


(defun print-node-append-time (node)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (node-append-time node))
    (format t "~a:~a:~a  ~a.~a.~a~%" hour min sec year month day)))



(defparameter *available-nodes* nil)
(defparameter *nodes-access-mutex* (sb-thread:make-mutex :name "pew-pew-pew"))

(defparameter *stop-slave-accepter* nil "set this var to 't' and connect to 'slave-accepter' to make it exit the infinite loop")
(defparameter *slave-accepter-thread* nil)





;; ok
(defun stop-slave-accepter ()
  (when (sb-thread:thread-alive-p *slave-accepter-thread*)
    (setf *stop-slave-accepter* t)
    (unwind-protect
	 (progn
	   (ws.network:with-tcp-stream (stream
					ws.config:*this-node-ip*
					ws.config:*default-master-port*)
	     (format stream "~a~%" 'cancel))
	   (sb-thread:join-thread *slave-accepter-thread*))
      (setf *stop-slave-accepter* nil))))



;; ok
(defun slave-cancel-p (slave-info)
  "slave-info is an info sent by slave. Returns true if info is CANCEL"
  (and (eql (type-of slave-info) 'symbol)
       (string= (symbol-name slave-info) "CANCEL")))



;; should be improved
(defun check-slave-info (slave-info)
  (let ((known-architectures (list "I686" "AMD64"))) ;; check this
    (and (listp slave-info)
	 (member (symbol-name (second (find-item "ARCHITECTURE" slave-info))) known-architectures :test 'equal) ;; is it necessary ?
	 (not (null (find-item "CPUS" slave-info)))
	 (not (null (find-item "MEM-FREE" slave-info)))
	 (not (null (find-item "MEM-TOTAL" slave-info))))))



;; mock
(defun is-node-duplicate (node1 node2)
  (flet ((ips-are-equal (node1 node2)
	   (and (= (length (node-ip node1))
		   (length (node-ip node2)))
		(let ((result t))
		  (dotimes (i (length (node-ip node1)))
		    (when (not (= (aref (node-ip node1) i)
				  (aref (node-ip node2) i)))
		      (setf result nil)
		      (return-from ips-are-equal nil)))
		  t)))
	 (ports-are-equal (node1 node2)
	   (= (node-port node1) (node-port node2))))
    (and (ips-are-equal node1 node2)
	 (ports-are-equal node1 node2))))



;; should be improved
;; Slave info should be in format '((LISTEN-PORT x) (ARCHITECTURE y) (CPUS z) (FPU-TEST a) (MEM-TOTAL b) (MEM-FREE c) (MEM-ACCESS D))
(defun admit-slave (slave-ip slave-info)
  (if (check-slave-info  slave-info)
      (let ((new-node (make-node-from-scratch slave-ip
					      (get-universal-time)
					      slave-info)))
	(sb-thread:with-recursive-lock (*nodes-access-mutex*)
	  (setf *available-nodes*
		(delete-if #'(lambda (node-in-list)
			       (is-node-duplicate node-in-list new-node))
			   *available-nodes*))
	  (setf *available-nodes*
		(cons new-node *available-nodes*))))
      (progn
	(format t "[ws.master::admit-slave] slave ~a sent unrecognizable info~%" slave-ip)
	nil)))



;; todo: split into 2 funcs, make with-timeout-handler macro
(defun start-slave-accepter ()
  (if  (and (not (null *slave-accepter-thread*))
	    (sb-thread:thread-alive-p *slave-accepter-thread*))
       (format t "[ws.master:start-slave-accepter] error: slave accepter thread is running~%")
       (setf *slave-accepter-thread*
	     (sb-thread:make-thread
	      (lambda ()
		(format t "[ws.master::slave-accepter] started~%")
		(let ((socket (make-instance 'sb-bsd-sockets:inet-socket
					     :type :stream
					     :protocol :tcp)))
		  (unwind-protect
		       (progn
			 (sb-bsd-sockets:socket-bind socket
						     ws.config:*this-node-ip*
						     ws.config:*default-master-port*)
			 (sb-bsd-sockets:socket-listen socket 30)
			 (do () (*stop-slave-accepter*)
			   (format t "[ws.master::slave-accepter] accepting clients at port ~a...~%"
				   ws.config:*default-master-port*)
			   (multiple-value-bind (client-socket client-address client-port)
			       (sb-bsd-sockets:socket-accept socket)
			     (let* ((client-stream (sb-bsd-sockets:socket-make-stream client-socket
										      :input t
										      :output t)))
			       (format t "[ws.master::slave-accepter] accepted new client:~a~a~%" client-address client-port)
			       (sb-thread:make-thread
				(lambda ()
				  (unwind-protect
				       (progn
					 (format t "[ws.master::slave-accepter] reading with timeout 5...~%")
					 (handler-case
					     (sb-ext:with-timeout 5
					       (let* ((slave-info nil))
						 (let ((*package* (find-package :cl-user)))
						   (setf slave-info (read client-stream)))
						 (format t "Accepted new slave:~a~%" slave-info)
						 (when (not (slave-cancel-p slave-info))
						   (if (admit-slave client-address slave-info)
						       (progn
							 (format client-stream "~a~%" 'OK)
							 (finish-output client-stream)
							 (format t "Slave ~a appended~%" client-address))
						       (progn
							 (format client-stream "~a~%" 'ERROR))))
						 (sb-bsd-sockets:socket-close client-socket)))
					   (sb-ext:TIMEOUT () (format t "[ws.master::slave-accepter] client ~a timeout~%"
								      client-address))
					   (END-OF-FILE () (format t "[ws.master::slave-accepter] client ~a SUDDENLY disconnected~%"
								   client-address))))
				    (sb-bsd-sockets:socket-close client-socket)
				    (format t "[ws.master::start-slave-accepter] client ~a disconnected~%"
					    client-address))))))))
		    (sb-bsd-sockets:socket-close socket)
		    (format t "[ws.master::slave-accepter] exiting~%"))))))))



;; mock
(defun find-suitable-nodes (predicate nodes-list)
  (let ((suitable-nodes nil))
    (dolist (item nodes-list)
      (when (funcall predicate item)
	(setf suitable-nodes
	      (cons item suitable-nodes))))
    suitable-nodes))
    


;; mock
(defmacro with-task ((task-name-str task-requirements sort-function client-part) &body body)
  "This macro provides following lexical bindings:
task-name - bound to first argument of this macro
suitable-nodes - bound to a list of nodes that passed task-requirements test
sortes-nodes - bound to sorted list of suitable-nodes by sort-function
task-code - bound to results of client-part form execution, will be sent to nodes
task-id - task identifier in ws.t-table"
  `(let* ((task-name ,task-name-str)
	  (suitable-nodes (find-suitable-nodes ,task-requirements *available-nodes*))
	  (sorted-nodes (sort (copy-list suitable-nodes) ,sort-function))
	  (task-code ,client-part)
	  (task-id (ws.t-table:insert-task)))
     (format t "suitable nodes: ~a~%" suitable-nodes)
     (format t "sorted nodes: ~a~%" sorted-nodes)
     ,@body))



;; mock, move to ws.network
(defmacro with-response-socket ((server-socket-name port) &body body)
  `(let* ((,server-socket-name (make-instance 'sb-bsd-sockets:inet-socket
					      :type :stream
					      :protocol :tcp))
	  (,port (bind-socket-to-free-port ,server-socket-name)))
     (unwind-protect
	  (progn
	    (sb-bsd-sockets:socket-listen ,server-socket-name 30)
	    ,@body)
       (sb-bsd-sockets:socket-close server-socket-name))))
       


;; mock should be deleted, end-point included into node
(defun node->end-point (node)
  (make-end-point :ip (node-ip node)
		  :port (node-port node)))



;; new, move to ws.protocol
(defmacro with-distribute-task ((task-code task-id task-name nodes-list respond-to) okay-clause fail-clause)
  "okay-clause and fail-clause should be functions"
  (let ((task-code-s (gensym))
	(task-id-s (gensym))
	(task-name-s (gensym))
	(nodes-list-s (gensym))
	(master-s (gensym))
	(node-iterator-s (gensym))
	(send-result-s (gensym)))
    `(let ((,task-code-s ,task-code)
	   (,task-id-s ,task-id)
	   (,task-name-s ,task-name)
	   (,nodes-list-s ,nodes-list)
	   (,master-s ,respond-to))
       (dolist (,node-iterator-s ,nodes-list-s)
	 (sb-thread:make-thread
	  #'(lambda ()
	      (let ((,send-result-s nil))
		(handler-case
		    (progn
		      (setf ,send-result-s
			    (ws.protocol:send-task ,task-code-s
						   ,task-id-s
						   ,task-name-s
						   ,master-s
						   (node->end-point ,node-iterator-s))))
		  (nil (some-exception)
		    (funcall ,fail-clause)))
		(when ,send-result-s
		  (funcall ,okay-clause)))))))))

'(with-distribute-task ('((format t "one more epik moment~%")
			 (format t "task to be executed:~a~%" task-code)
			 (format t "should respond to:~a~%" respond-to))
			(make-task-id :number 111)
			"task-name"
			*available-nodes*
			(make-end-point :ip (vector 127 0 0 1) :port 222))
  #'(lambda ()
      (format t "that wuz epik!!!11!~%"))
  #'(lambda ()
      (format t "lamdababada~%")))






'(execute-task (15 "mine uber task")
  (lambda (node)
    (and (> (node-cpu node) 1)
	(> (node-free-mem node) 400)))
 (lambda (node1 node2)
   (< (load-level node1) (load-level node2)))
 (client-part - a single form that evaluates to a task)
 (print *available-nodes*)
 (print *suitable-nodes*)
 (with-responce-socket (socket master-port)
   (distribute-task *suitable-nodes* master-port :master-ip (get-ip) :resources '( "1.jpg" "2.lisp" "3.tar.gz"))
   (with-slave-responce (socket slave slave-socket)
     "this code will be executed in separate thread for each connected slave"
     (do-something-1)
     (do-something-2))
   ))

(macroexpand-1 '(with-task ("task-1"
				    #'(lambda (node)
					(> (node-free-mem 128)))
				    #'(lambda (node1 node2)
					(> (node-free-mem node1)
					   (node-free-mem node2)))
				    '(format t "result:~a~%" (+ 2 3)))
			 (with-response-socket (socket master-port)
			   (with-distribute-task (task-code suitable-nodes master-port)
			     (okay-clause)
			     (fail-clause)) ;; if returns t, a new approach is undertaken
			   (with-slave-responce (server-socket slave slave-socket)
			     ;; interact with slave
			     ))))