(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.statistics
  (:nicknames :ws.stats)
  (:use :common-lisp
	:common-lisp-user
	:ws.g
	:ws.network)
  (:export :start-statistics-gathering
	   :stop-statistics-gathering
	   :start-statistics-sending))

(in-package :ws.stats)


(defparameter *stop-statistics-service* nil)





;; mock
(defun start-statistics-gathering (&optional (port ws.config:*default-master-statistics-port*))
  (with-tcp-server (client-stream client-end-point port '*stop-statistics-service*)
    (format t "received stats from ~a : ~a --> ~a~%"
	    (end-point-ip client-end-point)
	    (end-point-port client-end-point)
	    (read client-stream))))

;; mock
(defun stop-statistics-gathering (&optional (port ws.config:*default-master-statistics-port*))
  (with-tcp-connection (stream (make-end-point :ip ws.config:*this-node-ip*
					       :port port))
    (format stream "cancel~%")
    (finish-output stream)))

;; mock
(defun start-statistics-sending (&optional (master (make-end-point :ip ws.config:*default-master*
								   :port ws.config:*default-master-statistics-port*)))
  (do () (nil)
    (let ((load (read (sb-ext:process-output (sb-ext:run-program "/bin/sh"
								 `("load-stats.sh")
								 :output :stream)))))
      (with-tcp-connection (stream master)
	(format stream "~a~%" load)
	(finish-output stream)))
    (sleep 60)))
	    