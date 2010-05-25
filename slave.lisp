(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.slave
  (:nicknames :ws.slave)
  (:use :common-lisp
	:common-lisp-user)
  (:export :start-slave))

(in-package :ws.slave)





(defun start-slave (port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (unwind-protect
	 (progn
	   (sb-bsd-sockets:socket-bind socket
				       (vector 127 0 0 1)
				       ;;ws.protocol:*default-slave-port*)
				       port)
	   (sb-bsd-sockets:socket-listen socket 5)
	   (let* ((client-socket (sb-bsd-sockets:socket-accept socket))
		  (client-stream (sb-bsd-sockets:socket-make-stream client-socket
								    :input t
								    :output t))
		  (task nil))
	     ;; connection accepted. reading from stream...
	     (handler-case
		 (progn
		   (setf task (read client-stream))
		   (format t "accepted data:~a~%" task))
	       ('END-OF-FILE-ERROR () (progn
					(format t "sending bad format...~a~%"
						(ws.protocol:send-protocol-error client-socket 'bad-format)))))
	     (if (ws.protocol:single-task-p task)
		 (format client-stream "~a" 'TASK-OK)
		 (format client-stream "~a" 'bad-format))
	     (sb-bsd-sockets:socket-close client-socket)))
      (progn (sb-bsd-sockets:socket-close socket)
	     (format t  "unwind-protect:socket closed~%")))))