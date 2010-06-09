(defpackage h2s04.white-shadow.asd-system
  (:nicknames :ws.asd-system)
  (:use :common-lisp
	:common-lis-user
	:asdf))

(in-package :ws.asd-system)

(defsystem "h2s04.white-shadow"
    :description "White shadow was developed to automatically plan tasks in
a cluster or distributed system"
    :version "10.06.02"
    :author ".::[h2s04]::KIsLotnIK::."
    :license "GNU/GPL"
    :components ((:file "configuration")
		 (:file "log")
		 (:file "global-abstractions" :depends-on ("configuration"))
		 (:file "ports-table" :depends-on ("configuration"
						   "global-abstractions"
						   "log"))
		 (:file "network" :depends-on ("ports-table"
					       "global-abstractions"))
		 (:file "statistics" :depends-on ("configuration"
						  "global-abstractions"
						  "network"))
		 (:file "task-distribution-protocol" :depends-on ("configuration"
								  "global-abstractions"
								  "network"))))