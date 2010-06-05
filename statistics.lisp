(in-package :common-lisp-user)

(require 'sb-bsd-sockets)

(defpackage h2s04.white-shadow.statistics
  (:nicknames :ws.stats)
  (:use :common-lisp
	:common-lisp-user
	:ws.g)
  (:export :start-statistics-service
	   :stop-statistics-service))

(in-package :ws.stats)





