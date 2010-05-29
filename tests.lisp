(in-package :common-lisp-user)

(defpackage h2s04.white-shadow.tests
  (:nicknames :ws.tests)
  (:use :common-lisp
	:common-lisp-user)
  (:export :perform-tests-suite
	   :cpu-cores
	   :fpu-test
	   :mem-size
	   :mem-access
	   :executable-exists
	   :architecture))

(in-package :ws.tests)





(defun perform-tests-suite ()
  (multiple-value-bind (mem-total mem-free)
      (mem-size)
    (list `(listen-port ,ws.config:*default-slave-port*)
	  `(architecture ,(architecture))
	  `(cpus ,(cpu-cores))
	  `(fpu-test ,(fpu-test))
	  `(mem-total ,mem-total)
	  `(mem-free ,mem-free)
	  `(mem-access ,(mem-access)))))


(defun executable-exists (name)
  "Returns t or nil"
  (when (or (find #\space name)
	    (find #\tab name))
    (return-from executable-exists nil))
  (let* ((process (sb-ext:run-program "/bin/sh" `("-c"
						  ,(concatenate 'string
								"whereis -b "
								name))
				      :output :stream))
	 (line-responce (read-line (sb-ext:process-output process))))
    (when (search name line-responce :start2 1)
      t)))



(defun architecture ()
  "Returns string"
  (let* ((process (sb-ext:run-program "/bin/sh" `("-c" "uname -m")
				      :output :stream))
	 (line-responce (read-line (sb-ext:process-output process))))
    line-responce))



(defun cpu-cores ()
  "Returns number"
  (let ((process (sb-ext:run-program "/bin/sh" '("cpu-cores.sh") :output :stream)))
    (read (sb-ext:process-output process))))



(defun fpu-test ()
  "Returns number - ms of real-time execution"
  (let ((x 5)
	(y 2)
	(z 0))
    (dotimes (i 20000000)
      (setf z (/ x y)))))



(defun mem-size ()
  (let ((process (sb-ext:run-program "/bin/sh" '("mem-stats.sh") :output :stream)))
    (values (read (sb-ext:process-output process))
	    (read (sb-ext:process-output process)))))



(defun mem-access ()
  (let ((array (make-array '(2000 2000) :element-type 'integer :initial-element 0))
	(total-run-time 0))
    (sb-ext:call-with-timing #'(lambda (&rest rest)
				 (setf total-run-time
				       (second rest)))
			     (lambda ()
			       (dotimes (k 1000)
				 (dotimes (i 2000)
				   (dotimes (y 2000)
				     (aref array i y))))))
    total-run-time))