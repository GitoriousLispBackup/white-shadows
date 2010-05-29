(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.master
  (:nicknames :ws.master)
  (:use :common-lisp
	:common-lisp-user)
  (:export :start-slave-accepter
	   :stop-slave-accepter))

(in-package :ws.master)





(defstruct node
  (cpu-count 0)
  (mem-total 0)
  (mem-free 0)
  (architecture "I686")
  (ip nil)
  (port nil))



(defun find-item (item slave-tests)
  (find item slave-tests :test #'(lambda (item-to-search candidate)
				   (equal
				    (symbol-name (first candidate))
				    item-to-search))))



(defun make-node-from-tests (ip tests)
  (make-node :ip ip
	     :port (second (find-item "LISTEN-PORT" tests))
	     :cpu-count (second (find-item "CPUS" tests))
	     :mem-total (second (find-item "MEM-TOTAL" tests))
	     :mem-free (second (find-item "MEM-FREE" tests))
	     :architecture (symbol-name (second (find-item "ARCHITECTURE" tests)))))



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
  "slave-info is info sent by slave. Returns true if info is CANCEL"
  (and (eql (type-of slave-info) 'symbol)
       (string= (symbol-name slave-info) "CANCEL")))



;; mock
(defun check-slave-info (slave-info)
  (let ((known-architectures (list "I686" "AMD64"))) ;; check this
    (and (listp slave-info)
	 (member (symbol-name (second (find-item "ARCHITECTURE" slave-info))) known-architectures :test 'equal) ;; is it necessary ?
	 (not (null (find-item "CPUS" slave-info)))
	 (not (null (find-item "MEM-FREE" slave-info)))
	 (not (null (find-item "MEM-TOTAL" slave-info))))))



;; mock
(defun admit-slave (slave-ip slave-info)
  (if (check-slave-info  slave-info)
      (sb-thread:with-mutex (*nodes-access-mutex*)
	(setf *available-nodes*
	      (list (cons :slave-ip (cons slave-ip slave-info))
		    *available-nodes*)))
      (progn
	(format t "[ws.master::admit-slave] slave ~a sent unrecognizable info~%" slave-ip)
	nil)))

(defparameter *crap* nil)


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
						     ws.protocol:*default-master-port*)
			 (sb-bsd-sockets:socket-listen socket 30)
			 (do () (*stop-slave-accepter*)
			   (format t "[ws.master::slave-accepter] accepting clients at port ~a...~%"
				   ws.protocol:*default-master-port*)
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
						   (setf *crap* slave-info)
						   (format t "admit slave says:~a~%" (admit-slave client-address slave-info))
						   (if (admit-slave client-address slave-info)
						       (progn
							 (format client-stream "~a~%" 'OK)
							 (finish-output client-stream)
							 (format t "Slave appended~a~%" client-address))
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



;;(defun plan-task

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