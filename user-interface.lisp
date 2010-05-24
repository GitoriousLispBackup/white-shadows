(in-package :common-lisp-user)

(defpackage h2s04.white-shadow-ui
  (:nicknames :ws-ui)
  (:use :common-lisp :common-lisp-user))


(in-package :ws-ui)



(defparameter *max-thread-quantity-mutex*
  (sb-thread:make-mutex :name "threads quantity lock"))
(defparameter *max-threads-quantity* 1 "Maximum number of threads")


(defun set-max-threads-count (number)
  (assert (> number 0) (number) "Number of threads must be greater than zero")
  (sb-thread:with-mutex (*max-thread-quantity-mutex*)
    (setf *max-threads-quantity* number)))


(defun add-task (task)
  (setf *tasks-pool*
	(nconc *tasks-pool* task))
  (grab-new-task *tasks-pool* *threads-pool*))