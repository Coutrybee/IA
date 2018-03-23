;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 8
;;
(defun g-node-compare (node lst)
	(if lst
		(if (equal (node-state node) (node-state (first lst))) 
			(< (node-g node) (node-g (first lst)))					
			(g-node-compare node (rest lst)))
		t))

(defun graph-search-rec (lst_open lst_close problem strategy)
	(when lst_open
		(let ((node (first lst_open)))
			(cond 
				((funcall (problem-f-goal-test problem) node)
					node)
				((or 
					(member node lst_close :test-not (problem-f-search-state-equal problem)) 
					(g-node-compare node lst_close)
					)
						(graph-search-rec 
							(insert-nodes-strategy (expand-node node problem) (rest lst_open) strategy)
							(cons node lst_close)
							problem
							strategy))
				(t (graph-search-rec 
							(rest lst_open)
							lst_close
							problem
							strategy))))))

(defun graph-search (problem strategy) 
	(graph-search-rec 
		(list (make-node 
			:state (problem-initial-state problem) 
			:depth 0 
			:action nil
			:parent nil
			:g 0 
			:h (f-h-galaxy (problem-initial-state problem) *sensors*)
			:f (f-h-galaxy (problem-initial-state problem) *sensors*) ))
		nil
		problem
		strategy))
		
(defun a-star-search (problem)
    (graph-search problem *A-star*)) 
		
(graph-search *galaxy-M35* *A-star*)
(a-star-search *galaxy-M35*)