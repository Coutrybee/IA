;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 9
;;
(defun solution-path (node)
	(path-recur-cola node nil))

(defun path-recur-cola (node sol)
	(if node
		(path-recur-cola (node-parent node) (cons (node-state node) sol))
		sol))

(defun action-sequence (node)
	(action-recur-cola node nil)) 

(defun action-recur-cola (node sol)
	(if node
		(action-recur-cola (node-parent node) (cons (node-action node) sol))
		sol))

(action-sequence (a-star-search *galaxy-M35*))

(solution-path nil ) ;-> NIL 
(solution-path (a-star-search *galaxy-M35*))  ;;;-> (MALLORY ...) 