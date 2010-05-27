(in-package :common-lisp-user)

(defpackage h2s04.white-shadow.resource-manager
  (:nicknames :ws.res-man)
  (:use :common-lisp
	:common-lisp-user)
  (:export :load-resources-list
	   :insert-resource
	   :get-resources-for-task))





(defparameter *resources-list* nil)



;; ok
(defun load-resources-list ()
  (setf *resources-list* nil)
  (with-open-file (file-stream "res/res-list.lisp")
    (setf *resources-list* nil)
    (handler-case
	(do () (nil)
	  (setf *resources-list*
		(cons (read file-stream)
		      *resources-list*)))
      (END-OF-FILE () nil)))
  *resources-list*)





;; loader
(eval-when (:load-toplevel :execute)
  (load-resources-list))





;; ok
(defun insert-resource (file-path task-designator time-stamp)
  (with-open-file (file-stream "res/res-list.lisp"
			       :direction :output
			       :if-exists :append)
    (format file-stream "~a~%" (list file-path
				     task-designator
				     time-stamp)))
  (setf *resources-list* (list file-path
			       task-designator
			       time-stamp))
  *resources-list*)



;; ok
(defun get-resources-for-task (task-id)
  (let ((result nil))
    (dolist (item *resources-list*)
      (when (equal task-id (second item))
	(setf result
	      (cons item result))))
    result))