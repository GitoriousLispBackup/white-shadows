(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.slave
  (:nicknames :ws.slave)
  (:use :common-lisp
	:common-lisp-user
	:ws.g)
  (:export :start-slave
	   :connect-to-master))

(in-package :ws.slave)



(defparameter *current-master* nil "Holds ip of a master")
(defparameter *test-results* nil)





;; ok
(defun accept-binary-file (file-name file-size socket)
  (let ((buffer (make-array 600000
			    :element-type '(unsigned-byte 8)
			    :initial-element 0))
	(total 0))
    (with-open-file (file-stream file-name
				 :direction :output
				 :element-type '(unsigned-byte 8)
				 :if-exists :supersede)
      (format t "accept-binary-file: receiving...~%file-size should be:~a~%" file-size)
      (handler-case
	  (progn
	    (do ()
	      ((>= total file-size) nil) ;; end-case
	    (multiple-value-bind (data len host)
		(sb-bsd-sockets:socket-receive socket buffer nil)
	      (declare (ignore data))
	      (setf total (+ total len))
	      (format t "received some data, len:~a, total:~a, from:~a~%" len total host)
	      (when (= len 0)
		(format t "len is zero. sending total bytes...~%")
		(ws.protocol::send-printable-object socket total)
		(return))
	      (write-sequence buffer file-stream :end len)))
	    (ws.protocol::send-printable-object socket total))
	(END-OF-FILE () (format t "end of file catched~%"))))))



;; ok
(defun connect-to-master (&optional
			  (ip ws.config:*default-master*)
			  (port ws.config:*default-master-port*))
  (when (null *test-results*)
    (format t "[ws.slave:connect-to-master] -> no test results, performing test suite...~%")
    (setf *test-results*
	  (ws.tests:perform-test-suite)))
  (ws.network:with-tcp-connection (stream (make-end-point :ip ip
							  :port port))
    (format stream "~a~%" *test-results*)
    (finish-output stream)
    (handler-case
	(let ((server-answer (read stream)))
	  (cond
	    ((equal (symbol-name server-answer) "OK")
	     (setf *current-master* ip)
	     (format t "succesfully connected to master ~a : ~a~%" ip port))
	    ((equal (symbol-name server-answer) "ERROR")
	     (format t "master rejected this node~%"))
	    (t
	     (error "I've sent tests to master but received crap. Exiting..."))))
      (END-OF-FILE () (error "connection to master lost =(")))))



;; mock
(defun execute-task (task)
  (let ((task-id (make-task-id :end-point (make-end-point :ip (second (second task))
							  :port (second (third task)))
			       :number (second (fourth task))))
	(task-name (second (fifth task)))
	(respond-to (make-end-point :ip (second (sixth task))
				    :port (second (seventh task))))
	(task-code (cddddr (cdr (cdr (cdr task))))))
    (format t "code to be executed:~a~%" task-code)
    (finish-output)
    (eval `(progn ,@task-code))))



;; mock
(defun start-slave (&optional (port ws.config:*default-slave-port*))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp))
	(buffer (make-array 500
				     :element-type '(unsigned-byte 8)
				     :initial-element 0)))
    (unwind-protect
	 (progn
	   (sb-bsd-sockets:socket-bind socket ;; comment this out
				       ws.config:*this-node-ip*	;; this too
				       port) ;; and this, ofcourse :D
	   (sb-bsd-sockets:socket-listen socket 5)
	   ;;	   (sleep 5)
	   ;;	   (format t "task accepted, computing...")
	   ;;	   (finish-output)
	   ;;	   (sleep 17)
	   ;;	   (if (= ws.config:*default-slave-port* 30036)
	   ;;	     (format t "md5 collision at phrase \"password\"~%")
	   ;;	     (format t "failed to find collision~%"))
	   ;;	   (do () (nil)
	   ;;	     ())
	   (let* ((client-socket (sb-bsd-sockets:socket-accept socket))
		  (client-stream (sb-bsd-sockets:socket-make-stream client-socket
								    :input t
								    :output t))
		  (task nil))
	     ;; connection accepted. reading from stream...
	     (unwind-protect
		  (progn
		    (format t "slave started at port:~a~%" port)
		    (sb-bsd-sockets:socket-receive client-socket buffer 500)
		    (format t "data received from client:~a" buffer))
	       (sb-bsd-sockets:socket-close client-socket)
	       (sb-bsd-sockets:socket-close client-socket)
	       (format t "unwind-protect: client-socket closed~%"))))
      (progn (sb-bsd-sockets:socket-close socket)
	     (sb-bsd-sockets:socket-close socket)
	     (format t  "unwind-protect: slave socket closed~%")))))