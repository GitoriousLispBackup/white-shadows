(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.master
  (:nicknames :ws.master)
  (:use :common-lisp
	:common-lisp-user))

(in-package :ws.master)





(defstruct node
  (cpu-count 0)
  (total-mem 0)
  (free-mem 0)
  (architecture "i686")
  (ip nil)
  (port nil))


(defparameter *available-nodes* nil)
(defparameter *nodes-access-mutex* (sb-thread:make-mutex :name "pew-pew-pew"))

;; tests
(defun add-node ()
  (setf *available-nodes*
	(list (make-node :cpu-count 2
			 :total-mem (+ (random 1000000) 1000000)
			 :free-mem (random 1000000)
			 :architecture "i686"
			 :ip (vector 127 0 0 1)
			 :port ws.protocol:*default-slave-port*))))


(defun slave-accepter ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (unwind-protect
	 (progn
	   (sb-bsd-sockets:socket-bind socket
				       (vector 127 0 0 1)
				       ws.protocol:*default-master-port*)
	   (sb-bsd-sockets:socket-listen socket 30)
	   (do () (nil)
	     (format t "iteration...~%")
	     (let* ((client-socket (sb-bsd-sockets:socket-accept socket))
		    (client-stream (sb-bsd-sockets:socket-make-stream client-socket
								      :input t
								      :output t)))
	       (format t "making thread...~%")
	       (sb-thread:make-thread
		(lambda ()
		  (unwind-protect
		       (progn
			 (format t "inside thread. reading...~%")
			 (let* ((slave  (read client-stream)))
			   (format t "Accepted new slave:~a~%" slave)
			   (sb-bsd-sockets:socket-close client-socket)))
		    (sb-bsd-sockets:socket-close client-socket)))))))
      (sb-bsd-sockets:socket-close socket))))



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