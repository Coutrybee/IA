;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3
;;
(defun iguales (lst1 lst2)
	(if lst2
		(if (member (first lst2) lst1)
			(iguales lst1 (rest lst2))
			nil)
		t))

;;Devuelve una lista de todos los nombres de nodos mandatory por los que he pasado (incluye repetidos si los hay)
(defun comp-mandatory (node mandatory)
	(when node
		(if (member (node-state node) mandatory)
			(cons (node-state node) (comp-mandatory (node-parent node) mandatory))
			(comp-mandatory (node-parent node) mandatory))))

;;Si he pasado por todos los mandatory devuelve t sino nil
;;Devuelve t si mandatory es nil
(defun manda (node mandatory)
	(iguales (comp-mandatory node mandatory) mandatory))

(defun f-goal-test-galaxy (node planets-destination planets-mandatory)
	(and (member (node-state node) planets-destination) (manda node planets-mandatory)))
	
	
(defun f-search-state-equal-galaxy-recur (node planets-mandatory)
	(when node
		(if (member (node-state node) planets-mandatory :test 'equal)
			(cons (node-state node) (f-search-state-equal-galaxy-recur (node-parent node) planets-mandatory))
			(f-search-state-equal-galaxy-recur (node-parent node) planets-mandatory))))
	
(defun f-search-state-equal-galaxy (node_1 node_2 &optional planets-mandatory)   
	(when (equal (node-state node_1) (node-state node_2))
		(if planets-mandatory
			(equal (f-search-state-equal-galaxy-recur 
					node_1  
					planets-mandatory)
				(f-search-state-equal-galaxy-recur 
					node_2
					planets-mandatory))
			t)))




(f-search-state-equal-galaxy node-01 node-01) 
;-> T 
(f-search-state-equal-galaxy node-01 node-02) 
;-> NIL 
(f-search-state-equal-galaxy node-02 node-04) ;-> T 
 

(f-search-state-equal-galaxy node-01 node-01 '(Avalon)) ;-> T 
(f-search-state-equal-galaxy node-01 node-02 '(Avalon)) ;-> NIL 
(f-search-state-equal-galaxy node-02 node-04 '(Avalon)) ;-> T 
 

(f-search-state-equal-galaxy node-01 node-01 '(Avalon Katril)) ;-> T 
(f-search-state-equal-galaxy node-01 node-02 '(Avalon Katril)) ;-> NIL 
(f-search-state-equal-galaxy node-02 node-04 '(Avalon Katril)) ;-> NIL


(defparameter node-01
 (make-node :state 'Avalon) )
(defparameter node-02
 (make-node :state 'Kentares :parent node-01))
(defparameter node-03
 (make-node :state 'Katril :parent node-02))
(defparameter node-04
 (make-node :state 'Kentares :parent node-03))
(f-goal-test-galaxy node-01 '(Kentares Uranus) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-02 '(Kentares Uranus) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-03 '(Kentares Uranus) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-04 '(Kentares Uranus) '(Avalon Katril)); -> T