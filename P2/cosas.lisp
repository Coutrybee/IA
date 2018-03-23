;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem definition
;;
(defstruct problem
	 states ; List of states
	 initial-state ; Initial state
	 f-goal-test ; reference to a function that determines whether
	 ; a state fulfills the goal
	 f-h ; reference to a function that evaluates to the
	 ; value of the heuristic of a state
	 f-search-state-equal ; reference to a predicate that determines whether                        
							; two nodes are equal, in terms of their search state
	 operators) ; list of operators (references to functions)
	 ; to generate actions, which, in their turn, are
	 ; used to generate succesors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Node in the search algorithm
;;
(defstruct node
	state ; state label
	parent ; parent node
	action ; action that generated the current node from its parent
	 (depth 0) ; depth in the search tree
	 (g 0) ; cost of the path from the initial state to this node
	(h 0) ; value of the heuristic
	 (f 0)) ; g + h
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Actions
;;
(defstruct action
	name ; Name of the operator that generated the action
	origin ; State on which the action is applied
	final ; State that results from the application of the action
	cost ) ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Search strategies
;;
(defstruct strategy
	name ; Name of the search strategy
	node-compare-p) ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defparameter *planets* '(Avalon Davion Mallory Katril Kentares Proserpina Sirtis))
(defparameter *white-holes*
 '((Avalon Mallory 6.4) (Avalon Proserpina 8.6) 
	(Mallory Proserpina 15) (Mallory Katril 10) 
	(Katril Mallory 10) (Katril Davion 9)
	(Davion Proserpina 5) (Davion Sirtis 6)
	(Sirtis Davion 6) (Sirtis Proserpina 12)
	(Proserpina Davion 5) (Proserpina Sirtis 12) (Proserpina Mallory 15) (Proserpina Avalon 8.6)
	(Kentares Proserpina 7) (Kentares Katril 10) (Kentares Avalon 3)))

(defparameter *worm-holes*
 '((Avalon Kentares 4) (Avalon Mallory 9)
	(Kentares Avalon 4) (Kentares Proserpina 12)
	(Proserpina Kentares 12) (Proserpina Mallory 11) (Proserpina Sirtis 9)
	(Sirtis Proserpina 9) (Sirtis Davion 8) (Sirtis Katril 10)
	(Davion Katril 5) (Davion Sirtis 8)
	(Katril Davion 5) (Katril Mallory 5) (Katril Sirtis 10)
	(Mallory Proserpina 11) (Mallory Katril 5) (Mallory Avalon 9))) 



(defparameter *sensors*
 '((Avalon 15) 
	(Mallory 12)
	(Kentares 14)
	(Davion 5)
	(Proserpina 7)
	(Katril 9)
	(Sirtis 0)))


(defparameter *planet-origin* 'Mallory)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden* '(Avalon))
(defparameter *planets-mandatory* '(Katril Proserpina))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1
;;
(defun f-h-galaxy (state sensors)
	(when sensors
		(if (equal state (first (first sensors)))
			(second (first sensors))
			(f-h-galaxy state (rest sensors)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 2
;;
(defun navigate (name state net planets-forbidden)
    (let ((primero (first net))
            (resto (rest net)))
        (when net
            (cond ((member (second primero) planets-forbidden)
                    (navigate name state resto planets-forbidden))
                    ((equal state (first (first net)))
                        (cons (make-action :name name :origin state :final (second primero) :cost (third primero)) (navigate name state resto planets-forbidden)))
                    (t (navigate name state resto planets-forbidden))))))


(defun navigate-white-hole (state white-holes )
    (navigate 'NAVIGATE-WHITE-HOLE state white-holes nil))


(defun navigate-worm-hole (state worm-holes planets-forbidden)
    (navigate 'NAVIGATE-WORM-HOLE state worm-holes planets-forbidden))
    
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 4
;;
(defparameter *galaxy-M35*
    (make-problem
        :states *planets*
        :initial-state *planet-origin*
        :f-goal-test #'(lambda (node)
                (f-goal-test-galaxy node *planets-destination* 
                    *planets-mandatory*))
        :f-search-state-equal #'(lambda (node-1 node-2)
                (f-search-state-equal-galaxy node-1 node-2 *planets-mandatory*)) 
        :f-h #'(lambda (state) (f-h-galaxy state *sensors*))
        :operators (list  
                        #'(lambda (state)
                            (navigate-worm-hole state *worm-holes* *planets-forbidden*))
                        #'(lambda (state)
                            (navigate-white-hole state *white-holes*))))) ;;Van las funciones de navigate
                 
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 5
;;
(defun list_actions (node operators)
	(mapcan #'(lambda (op)
				(funcall op (node-state node)))
				operators))

(defun get_states (node actions f-h)
	(mapcar	#'(lambda (action)
				(make-node 
					:state (action-final action) 
					:parent node 
					:action action 
					:depth (+ (node-depth node) 1) 
					:g (+ (action-cost action) (node-g node))
					:h (funcall f-h (action-final action))
					:f (+ (+ (action-cost action) (node-g node))
						(funcall f-h (action-final action)))))
		actions))
		
(defun expand-node (node problem)
	(get_states node 
		(list_actions node (problem-operators problem)) 
		(problem-f-h problem)))

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 6
;;
(defun insert_node (node lst-nodes strategy)
	(if lst-nodes
		(if (funcall strategy node (first lst-nodes))
			(cons node lst-nodes)
			(cons (first lst-nodes) (insert_node node (rest lst-nodes) strategy)))
		(cons node nil)))
		
(defun insert-nodes-strategy (nodes lst-nodes strategy)
	(if nodes
		(insert-nodes-strategy (rest nodes) 
			(insert_node (first nodes) lst-nodes (strategy-node-compare-p strategy))
			strategy)
		lst-nodes))
		



(defun node-g-<= 
	(node-1 node-2)   
	(<= (node-g node-1)       
	(node-g node-2)))

(defparameter *uniform-cost*   
	(make-strategy 
		:name 'uniform-cost 
		:node-compare-p #'node-g-<=)) 
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 7
;;
(defun node-f-<= 
	(node-1 node-2)   
	(<= (node-f node-1)       
	(node-f node-2)))

(defparameter *A-star*   
	(make-strategy 
		:name 'uniform-cost 
		:node-compare-p #'node-f-<=))
		
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
					(member node lst_close :test-not #'equal) 
					(g-node-compare node lst_close))
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
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 9
;;


(defun path-recur-cola (node sol)
	(if node
		(path-recur-cola (node-parent node) (cons (node-state node) sol))
		sol))

(defun solution-path (node)
	(path-recur-cola node nil))



(defun action-recur-cola (node sol)
	(if node
		(action-recur-cola (node-parent node) (cons (node-action node) sol))
		sol))

(defun action-sequence (node)
	(action-recur-cola node nil)) 

