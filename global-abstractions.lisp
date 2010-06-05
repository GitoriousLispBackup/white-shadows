;; depends-on: ws.config

;; Global abstractions include following abstractions:
;;  end-point - a place that can be connected to, usually an ip-address and a port
;;  task-id   - object that uniquelly identifies a task in all means


(in-package :common-lisp-user)

(defpackage h2s04.white-shadow.global-abstractions
  (:nicknames :ws.g)
  (:use :common-lisp
	:common-lisp-user)
  (:export :end-point
	   :make-end-point
	   :end-point-ip
	   :end-point-port

	   :task-id
	   :make-task-id
	   :task-id-end-point
	   :task-id-number))

(in-package :ws.g)



(defstruct end-point
  (ip nil)
  (port nil))

(defstruct task-id
  (end-point (make-end-point :ip ws.config:*this-node-ip*))
  (number nil))

;;(defstruct node
