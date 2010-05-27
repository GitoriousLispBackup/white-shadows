(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.slave
  (:nicknames :ws.slave)
  (:use :common-lisp
	:common-lisp-user)
  (:export :start-slave))

(in-package :ws.slave)





;; mock
(defun accept-binary-file (file-name socket)
  (let ((buffer (make-array 600000
			    :element-type '(unsigned-byte 8)
			    :initial-element 0)))
    (with-open-file (file-stream file-name
				 :direction :output
				 :element-type '(unsigned-byte 8)
				 :if-exists :supersede)
      (format t "accept-binary-file: receiving...~%")
      (handler-case
	  (do ((i 0 (1+ i))
	       (total 0))
	      ((> i 30) nil) ;; end-case
	    (multiple-value-bind (data len host)
		(sb-bsd-sockets:socket-receive socket buffer nil)
	      (declare (ignore data))
	      (setf total (+ total len))
	      (format t "received some data, len:~a, total:~a, from:~a~%" len total host)
	      (when (= len 0)
		(return)))
	    (write-sequence buffer file-stream))
	(END-OF-FILE () (format t "end of file catched~%"))))))





;; mock
(defun start-slave (port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (unwind-protect
	 (progn
	   (sb-bsd-sockets:socket-bind socket
				       (vector 127 0 0 1)
				       port)
	   (sb-bsd-sockets:socket-listen socket 5)
	   (let* ((client-socket (sb-bsd-sockets:socket-accept socket))
		  (client-stream (sb-bsd-sockets:socket-make-stream client-socket
								    :input t
								    :output t))
		  (buffer (make-array 55
				      :initial-element 0
				      :element-type '(unsigned-byte 8)))
		  (task nil))
	     ;; connection accepted. reading from stream...
	     (format t "slave started at port:~a~%" port)
	     (handler-case
		 (progn
		   (setf task (read client-stream))
		   (format t "accepted data:~a~%" task))
	       (END-OF-FILE () (progn
				 (format client-stream "~a~%" 'bad-format))))
	     (finish-output)
	     (cond
	       ((ws.protocol:single-task-p task)
		(format client-stream "~a~%" 'TASK-OK))
	       ((ws.protocol:resource-p task)
		(format t "received resource~%")
		(format client-stream "~a~%" 'RESOURCE-OK)
		(finish-output client-stream)
		(format t "receiving binary data...~%")
		(accept-binary-file (concatenate 'string
						 "res/"
						 (second task))
				    client-socket))
	       (t (format client-stream "~a~%" 'bad-format)))
	     (sb-bsd-sockets:socket-close client-socket)))
      (progn (sb-bsd-sockets:socket-close socket)
	     (format t  "unwind-protect:socket closed~%")))))