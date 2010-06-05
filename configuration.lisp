(in-package :common-lisp-user)

(defpackage h2s04.white-shadow.configuration
  (:nicknames :ws.config)
  (:use :common-lisp
	:common-lisp-user)
  (:export :*this-node-ip*
	   :*default-master*
	   :*default-master-statistics-port*
	   :*default-master-port*
	   :*default-slave-port*))

(in-package :ws.config)





(defparameter *this-node-ip* (vector 127 0 0 1) "This node ip")

(defparameter *default-master* (vector 127 0 0 1) "Default master ip for current slave")


(defparameter *default-master-statistics-port* 30031 "Default master's port that collects statistics")

(defparameter *default-master-port* 30032 "Default master's listening port")

(defparameter *default-slave-port* 30033 "Default slave's listening port")