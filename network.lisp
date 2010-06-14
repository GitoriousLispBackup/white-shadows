;; depends on: ws.ports-table, global-abstractions

(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.network
  (:nicknames :ws.network)
  (:use :common-lisp
	:common-lisp-user
	:ws.g)
  (:export :with-tcp-connection
	   :with-tcp-server))

(in-package :ws.network)





;; ok
(defmacro with-tcp-connection ((socket-stream-name end-point) &body body)
  "varlist: (stream-var ip port)"
  (let ((socket (gensym))
	(end-point-s (gensym)))
    `(let ((,socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
	   (,end-point-s ,end-point))
       (unwind-protect
	    (progn
	      (sb-bsd-sockets:socket-connect ,socket
					     (end-point-ip ,end-point-s)
					     (end-point-port ,end-point-s))
	      (let ((,socket-stream-name (sb-bsd-sockets:socket-make-stream ,socket
									    :input t
									    :output t)))
		,@body))
	 (sb-bsd-sockets:socket-close ,socket)))))





;; mock
(defmacro with-tcp-server ((client-stream-name client-addr-name accept-port stop-condition) &body body)
  (let ((accept-port-s (gensym))
	(stop-condition-s (gensym))
	(server-socket-s (gensym))
	(client-socket-s (gensym))
	(client-addr-s (gensym))
	(client-port-s (gensym)))
    `(let ((,accept-port-s ,accept-port)
	   (,stop-condition-s ,stop-condition)
	   (,server-socket-s (make-instance 'sb-bsd-sockets:inet-socket
					    :type :stream
					    :protocol :tcp)))
       (sb-thread:make-thread
	#'(lambda ()
	    (unwind-protect
		 (progn
		   (setf (sb-bsd-sockets:sockopt-reuse-address ,server-socket-s) t)   ;; wath this
		   (sb-bsd-sockets:socket-bind ,server-socket-s
					       ws.config:*this-node-ip*
					       ,accept-port-s)
		   (sb-bsd-sockets:socket-listen ,server-socket-s 5)
		   (do () ((symbol-value ,stop-condition-s))
		     (multiple-value-bind (,client-socket-s ,client-addr-s ,client-port-s)
			 (sb-bsd-sockets:socket-accept ,server-socket-s)
		       (sb-thread:make-thread
			#'(lambda ()
			(unwind-protect
			     (let ((,client-stream-name (sb-bsd-sockets:socket-make-stream ,client-socket-s
											   :input t
											   :output t))
				   (,client-addr-name (make-end-point :ip ,client-addr-s
								      :port ,client-port-s)))
			       ,@body)
			  (sb-bsd-sockets:socket-close ,client-socket-s)))))))
	      (sb-bsd-sockets:socket-close ,server-socket-s)))))))