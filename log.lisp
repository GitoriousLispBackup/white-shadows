(in-package :common-lisp-user)

(defpackage h2s04.white-shadow.log
  (:nicknames :ws.log)
  (:use :common-lisp-user
	:common-lisp)
  (:export :log-to
	   :flush-logs))

(in-package :ws.log)


;; Log-file for developer's team
(defparameter *dev-log* (open "logs/dev-team.log"
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create))

;; Log-file for user
(defparameter *usr-log* (open "logs/usr.log"
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create))

;; Log-file that receives all program's feelings
(defparameter *fns-log* (open "logs/feelings.log"
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create))



;; mock
(defun log-to (place control-string &rest rest)
  "Possible places :dev :usr :fns"
  (format (cond
	    ((eq place :dev) *dev-log*)
	    ((eq place :usr) *usr-log*)
	    ((eq place :fns) *fns-log*)
	    (t (error "ws.log:log-to -> received unknown place")))
	  control-string
	  rest))



;; mock
(defun flush-logs ()
  (finish-output *dev-log*)
  (finish-output *usr-log*)
  (finish-output *fns-log*))