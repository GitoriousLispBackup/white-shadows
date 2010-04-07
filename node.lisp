;; Object sending is provided by function SEND-TASK
;; Object receiving and execution are provided by function MAKE-ECHOER

;; How to test:
;;    LOAD this file
;;    Open *inferior-lisp* buffer in new window to watch results of execution
;;    Open REPL and type (ws-node::send-task ws-node::*node1* '(+ 2 3))
;;    You will see some movement in the *inferior-lisp* window
;;    Any READable lisp objects may be sent on execution



(in-package :common-lisp-user)

(defpackage h2s04.white-shadow-node
  (:nicknames :ws-node)
  (:use :common-lisp
	:common-lisp-user
	:sb-bsd-sockets))


(in-package :ws-node)



(defparameter *port* 30033)



(defun make-echoer (stream id disconnector)
  (lambda (_)
    (declare (ignore _))
    (handler-case
        (let ((cmd (read stream)))
	  (cond ((equal cmd '(quit))
                 (funcall disconnector))
                (t
		 (format t "evaling: ~a~%" cmd)
		 (let ((result (eval cmd)))
		   (format t "~a: ~a~%" id result)
		   (format stream "~a: ~a~%" id result))
                 (force-output stream))))
      (end-of-file ()
        (funcall disconnector)))))

(defun make-disconnector (socket id)
  (lambda ()
    (let ((fd (socket-file-descriptor socket)))
      (format t "~a: closing~%" id)
      (sb-impl::invalidate-descriptor fd)
      (socket-close socket))))

(defun serve (socket id)
  (let ((stream (socket-make-stream socket :output t :input t))
        (fd (socket-file-descriptor socket)))
    (sb-impl::add-fd-handler fd
                             :input
                             (make-echoer stream
                                          id
                                          (make-disconnector socket id)))))

(defun echo-server (&optional (port *port*))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        (counter 0))
    (socket-bind socket (vector 127 0 0 1) port)
    (socket-listen socket 5)
    (sb-impl::add-fd-handler (socket-file-descriptor socket)
                             :input
                             (lambda (_)
                               (declare (ignore _))
                               (incf counter)
                               (format t "Accepted client ~A~%" counter)
                               (serve (socket-accept socket) counter)))))

#+sb-thread
(sb-thread:make-thread (lambda ()
                         (echo-server)
                         (loop
                            (sb-impl::serve-all-events))))
#-sb-thread
(echo-server)


;; as a tool

(defun nslookup (hostname)
  "Performs a DNS look up for HOSTNAME and returns the address as a
  four element array, suitable for socket-connect. If HOSTNAME is
  not found, a host-not-found-error condition is thrown."
  (if hostname
      (host-ent-address (get-host-by-name hostname))
      nil))


;; connect to a server

(defun tcp-connect (server port &optional (timeout 5))
  "Returns a socket connected to SERVER:PORT. If an error occurs, or the attempt times out
  after TIMOUT (default 5) secons, nil is returned."
  (when (and server port)
    (handler-case
	(let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
	  (socket-connect socket (nslookup server) port)
	  socket)
      (host-not-found-error ()
	(format t "host ~A not found." server)
	(force-output)
	nil))))


(defparameter *node1* (tcp-connect "127.0.01" 30033))


(defun send-task (socket task)
  (let ((task-str (concatenate 'string (prin1-to-string task) (list #\return #\newline))))
	   (sb-bsd-sockets:socket-send socket task-str (length task-str))))