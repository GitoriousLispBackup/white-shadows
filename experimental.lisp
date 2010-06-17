(with-task ((gen-task-name)
		     #'(lambda (node)
			 t)
		     #'(lambda (node1 node2)
			 nil)
		     (some nodes connection info or algos)
		     (task))) ;; no server part

(defpiece "name" 'token-green



(defstruct piece
  node
  launch-time
  status ;; possible statuses: 'LAUNCHED 'EXITED


(defstruct task
  name
  id
  status
  spawn-time

(defun spawn-task (task-name nodes-filter nodes-sort)
  (make-instance 'task
		 :name task-name
		 :id (make-task-id)
		 :