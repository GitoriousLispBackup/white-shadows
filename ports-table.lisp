(in-package :common-lisp-user)

(require :sb-bsd-sockets)

(defpackage h2s04.white-shadow.ports-table
  (:nicknames :ws.p-table)
  (:use :common-lisp
	:common-lisp-user)
  (:export "bind-socket-to-free-port"))

(in-package :h2s04.white-shadow.ports-table)





(defparameter *first-used-port* 30050 "First used port")
(defparameter *list-of-used-ports* nil "List of all used ports")

(defparameter *table-access-mutex* (sb-thread:make-mutex :name "pew-pew"))


;; internal utility functions
;;---------------------------

;; ok
(defun find-free-port ()
  "WARNING !!! NOT THREAD-SAFE !!! FOR INTERNAL USE ONLY !!!"
  (let ((port *first-used-port*))
    (when (null *list-of-used-ports*) ;; if no used ports
      (setf *list-of-used-ports* (list port))
      (return-from find-free-port port))
    (do ()
	((not (find port *list-of-used-ports*)) port)
      (incf port))
    (setf *list-of-used-ports* (nconc (list port)
				      *list-of-used-ports*))
    port))



;; ok
(defun remove-port-from-table (port)
  "WARNING !!! NOT THREAD-SAFE !!! FOR INTERNAL USE ONLY !!!"
  (setf *list-of-used-ports* (delete port *list-of-used-ports*)))



;; ok
(defun try-to-bind (socket port)
  (sb-bsd-sockets:socket-bind socket (vector 127 0 0 1) port))





;; exported functions
;;-------------------

;; ok
(defun bind-socket-to-free-port (socket)
  "Searches for a free port starting with *first-used-port*. Adds found port to the *list-of-used-ports*. After that tries to bind socket to the port. In case of error searches for a new port and removes current port from the list."
  (sb-thread:with-recursive-lock (*table-access-mutex*)
    (let ((free-port (find-free-port)))
      (handler-case
	  (progn
	    (try-to-bind socket free-port) ;; bind
	    free-port)                     ;; return free-port if bind succeeded
	(SB-BSD-SOCKETS:ADDRESS-IN-USE-ERROR () ;; if error than find a new one
	  (let ((new-free-port (bind-socket-to-free-port socket))) ;; recursion
	    (remove-port-from-table free-port)
	    new-free-port))))))