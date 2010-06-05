;; depends on: ws.ports-table, global-abstractions

(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.network
  (:nicknames :ws.network)
  (:use :common-lisp
	:common-lisp-user)
  (:export :with-tcp-stream))

(in-package :ws.network)





;; mock
(defmacro with-tcp-stream (varlist &body body)
  "varlist: (stream-var ip port)"
  (let ((socket (gensym))
	(address-param (gensym))
	(port-param (gensym)))
    `(let ((,address-param ,(second varlist))
	   (,port-param ,(third varlist))
	   (,(first varlist) nil))
       (declare (ignore ,(first varlist)))
       (unwind-protect
	    (progn
	      (setf ,socket (make-instance 'sb-bsd-sockets:inet-socket
					   :type :stream
					   :protocol :tcp))
	      (sb-bsd-sockets:socket-connect ,socket
					     ,address-param
					     ,port-param)
	      (let ((,(first varlist) (sb-bsd-sockets:socket-make-stream ,socket
									:input t
									:output t)))
		,@body))
	 (finish-output ,(first varlist))
	 (sb-bsd-sockets:socket-close ,socket)))))