;; this file contains helper functions
;; that assist program development


(in-package :common-lisp-user)

(load "task-distribution-protocol.lisp")
(load "slave.lisp")

(setf gport 30000)



(defun srv ()
  (sb-thread:make-thread (lambda ()
			   (handler-case
			       (ws.slave:start-slave gport)
			     (SB-BSD-SOCKETS:ADDRESS-IN-USE-ERROR ()
			       (progn (decf common-lisp-user::gport)
				      (srv)))))))



(defun tst ()
	   (ws.protocol:send-resource "103.jpg" "103.jpg" (ws.protocol:make-node :ip (vector 127 0 0 1)
										 :port gport)))


(defun ss ()
  (sb-thread:make-thread
   (lambda ()
     (handler-case
	 (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
				      :type :stream
				      :protocol :tcp)))
	   (sb-bsd-sockets:socket-bind socket
				       (vector 127 0 0 1)
				       gport)
	   (sb-bsd-sockets:socket-listen socket 5)
	   (let ((client-socket (sb-bsd-sockets:socket-accept socket))
		 (buffer (make-array 5
				     :element-type '(unsigned-byte 8)
				     :initial-element 0)))
	     (handler-case
		 (do () (nil)
		   (format t "~a bytes received~%"
			   (sb-bsd-sockets:socket-receive client-socket buffer 5)) ;;check 5
		   (format t "contents:~a~%" buffer))
	       (end-of-file () (sb-bsd-sockets:socket-close client-socket)))
	     (sb-bsd-sockets:socket-close socket)))
       (SB-BSD-SOCKETS:ADDRESS-IN-USE-ERROR ()
	 (progn (decf common-lisp-user::gport)
		(srv)))))))

(defun gg ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp))
	(buffer (make-array 93 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i (length buffer))
      (setf (elt buffer i)
	    (random 22)))
    (sb-bsd-sockets:socket-connect socket (vector 127 0 0 1) gport)
    (sb-bsd-sockets:socket-send socket buffer (length buffer))
    (sb-bsd-sockets:socket-send socket buffer (length buffer))
    (sb-bsd-sockets:socket-send socket buffer (length buffer))))