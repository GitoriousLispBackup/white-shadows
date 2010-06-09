;; depends on : ws.config, sb-thread, ws.log, ws.g

(in-package :common-lisp-user)


(defpackage h2s04.white-shadow.task-table
  (:nicknames :ws.t-table)
  (:use :common-lisp
	:common-lisp-user
	:ws.g)
  (:export :insert-task))

(in-package :ws.t-table)



(defparameter *last-task-number* 0)
(defparameter *last-task-number-mutex* (sb-thread:make-mutex :name "last-task-id-mutex"))


(defun this-end-point ()
  (make-end-point :ip ws.config:*this-node-ip*
		  :port ws.config:*default-master-port*))


;; ok
(defun generate-task-id ()
  (sb-thread:with-mutex (*last-task-number-mutex*)
    (make-task-id :end-point (this-end-point)
		  :number (incf *last-task-number*))))



;; each task is uniquelly identified by this structure
;;(defstruct task-id
;;  (ip ws.config:*this-node-ip*)           ;; ip address of the master that created the task
;;  (port ws.config:*default-master-port*)  ;; port of the master that created the task
;;  (number nil))                           ;; translate#1: sequence-number of the task, must be unique in context of ip-address:port



;; ok
(defun vector= (vector1 vector2)
  (and (= (length vector1) (length vector2))
       (do ((iterator 0 (1+ iterator)))
	   ((= iterator (length vector1)) t)
	 (when (not (= (aref vector1 iterator)
		       (aref vector2 iterator)))
	   (return-from vector= nil)))))



;; ok
(defun task-ids-are-equal (id1 id2)
  (and (= (task-id-number id1)
	  (task-id-number id2))
       (= (end-point-port (task-id-end-point id1))
	  (end-point-port (task-id-end-point id2)))
       (vector= (end-point-ip (task-id-end-point id1))
		(end-point-ip (task-id-end-point id2)))))



;; ok
(defstruct task
  (id (generate-task-id))
  (name nil)
  (status 'not-launched) ;; possible statuses: 'not-launched 'in-progress 'finished 'execution-error
  (creation-time (get-universal-time))
  (launch-time nil)
  (finish-time nil))



(defparameter *working-tasks* (make-array 1 :initial-element nil :adjustable t :fill-pointer 0) "Array of all tasks with status 'not-launched or 'in-progress")
(defparameter *finished-tasks* (make-array 1 :initial-element nil :adjustable t :fill-pointer 0) "Array of all tasks with status 'finished or 'execution-error")
(defparameter *all-task-tables-mutex* (sb-thread:make-mutex :name "*all-tasks-mutex*"))



;; new
(defun insert-task (name)
  "Creates new task with specified name and inserts it into the *working-tasks*. Returns task id"
  (let* ((new-task (make-task :name name))
	 (collision nil))
    (block collision-detection
      (sb-thread:with-mutex (*all-task-tables-mutex*)
	(setf collision (position new-task
				  *working-tasks*
				  :test #'(lambda (new-task item-in-seq)
				     (task-ids-are-equal (task-id new-task)
							 (task-id item-in-seq)))))
	(when collision
	  (setf (aref *working-tasks* collision) new-task)
	  (return-from collision-detection))
	(vector-push-extend new-task *working-tasks*)))
    (when collision
      (format t "ws.t-table : insert-task -> collision, old task deleted~%")
      (ws.log:log-to :dev "Internal error in h2s04.white-shadow.task-table : insert-task : collision occured o_O, previous task will be superseded.")
      (ws.log:log-to :usr "Looks like some error occured in ws.t-table:insert-task - duplicate tasks. Please send a notice to developers. Thank you =)."))
    (task-id new-task)))
