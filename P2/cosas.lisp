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




(defparameter *planets* '(Avalon Davion Mallory 
                            Katril Kentares Proserpina Sirtis))
(defparameter *white-holes*
    '((Avalon Mallory 6.4) (Avalon Proserpina 8.6) 
        (Mallory Proserpina 15) (Mallory Katril 10) 
        (Katril Mallory 10) (Katril Davion 9)
        (Davion Proserpina 5) (Davion Sirtis 6)
        (Sirtis Davion 6) (Sirtis Proserpina 12)
        (Proserpina Davion 5) (Proserpina Sirtis 12) 
        (Proserpina Mallory 15) (Proserpina Avalon 8.6)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1
;;
;;
;; Devuelve el valor de heuristica de un estado 
;; desde Mallory
;;
;;

(defun f-h-galaxy (state sensors)
	(when sensors
		(let ((sensor-first (first sensors)))
			(if (equal state (first sensor-first))
				(second sensor-first)
				(f-h-galaxy state (rest sensors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 2
;;
;;
;; Devuelve el conjunto de acciones desde un estado
;; 
;;

(defun navigate (name state net planets-forbidden)
    (let ((primero (first net))
            (resto (rest net)))
        (when net                                                       ; Si no queda ningun nodo devuelvo nil
            (cond 
                ((member (second primero) planets-forbidden)            ; Si el estado destino esta prohibido no hago nada
                    (navigate name state resto planets-forbidden))
                ((equal state (first primero))                          ; Si el el estado origen es el mismo que el dado creo 
                    (cons (make-action                                  ; un nodo y lo concateno con la salida recursiva
                            :name name 
                            :origin state 
                            :final (second primero) 
                            :cost (third primero)) 
                        (navigate name state resto planets-forbidden)))
                (t (navigate name state resto planets-forbidden))))))   ; En otro caso sigo buscando


(defun navigate-white-hole (state white-holes )
    (navigate 'NAVIGATE-WHITE-HOLE state white-holes nil))


(defun navigate-worm-hole (state worm-holes planets-forbidden)
    (navigate 'NAVIGATE-WORM-HOLE state worm-holes planets-forbidden))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Comprueba si el nodo es un nodo final
;; 
;;

(defun f-goal-test-galaxy-recur (node planets-mandatory)
	(if node											
		(if planets-mandatory								; Si he pasado por todos los obligados devuelvo t
			(f-goal-test-galaxy-recur 
				(node-parent node) 
				(let ((state (node-state node)))
					(if (member state planets-mandatory)	; Si el nodo esta en los obligados, lo quito de la lista
						(set-exclusive-or 
							(list state) 
							planets-mandatory)
						planets-mandatory)))
			t)
		(not planets-mandatory)))							; Si no hay mas nodos en el camino devuelvo los planetas 
															; obligados que quedan por visitar
										
			
(defun f-goal-test-galaxy (node planets-destination planets-mandatory)
	(and (member (node-state node) planets-destination) 
		(f-goal-test-galaxy-recur node planets-mandatory)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Comprueba si los dos nodos han pasado por los mismos
;; nodos obligatorios
;;
;;

(defun f-search-state-equal-galaxy-recur (node planets-mandatory)
	(when (and node planets-mandatory)					; Si no quedan nodos en el camino o si no quedan planetas obligados, devuelvo nil
		(let ((parent (node-parent node))
				(state (node-state node)))
			
			(if (member state							
					planets-mandatory 
					:test 'equal)
				(cons state 							; Si el nodo actual esta en la lista de obligados 
					(f-search-state-equal-galaxy-recur	; lo concateno con la llamada recursiva y lo extraigo de la lista
						parent  
						(set-exclusive-or 
							(list state) 
							planets-mandatory)))
				(f-search-state-equal-galaxy-recur		; Si no esta en la lista de obligados 
					parent								; sigo recorriendo el camino con la llamada recursiva
					planets-mandatory)))))
	
(defun f-search-state-equal-galaxy (node_1 node_2 &optional planets-mandatory)   
	(when (equal (node-state node_1)					; Si los nodos no tienen el mismo estado devuelvo nil
			(node-state node_2))	
		(if planets-mandatory							
			(not (set-exclusive-or						; Si las listas de nodos obligados por los que ha pasado
					(f-search-state-equal-galaxy-recur	; tienen los mismo elementos, devuelvo nil
						node_1  
						planets-mandatory)
					(f-search-state-equal-galaxy-recur 
						node_2
						planets-mandatory)))
			t)))										; Si no hay nodos obligados devuelvo t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 4
;;
;;
;; Define la galaxia m35 como un problema
;; 
;;

(defparameter *galaxy-M35*
    (make-problem
        :states *planets*
        :initial-state *planet-origin*
        :f-goal-test 
            #'(lambda (node)
                (f-goal-test-galaxy node 
                    *planets-destination* 
                    *planets-mandatory*))
        :f-search-state-equal 
            #'(lambda (node-1 node-2)
                (f-search-state-equal-galaxy 
                    node-1 
                    node-2 
                    *planets-mandatory*)) 
        :f-h #'(lambda (state) 
                (f-h-galaxy state *sensors*))
        :operators 
            (list  
                #'(lambda (state)
                    (navigate-worm-hole 
                        state 
                        *worm-holes* 
                        *planets-forbidden*))
                #'(lambda (state)
                    (navigate-white-hole 
                        state 
                        *white-holes*))))) 
                 
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 5
;;
;;
;; Devuelve una lista con los nodos a los que puede acceder un nodo
;; 
;;

(defun list_actions (node operators)			; Lista las acciones de un nodo para cada una de las operaciones
	(mapcan 
		#'(lambda (op)
			(funcall op (node-state node)))
			operators))

(defun get_states (node actions f-h)			; Para cada accion crea el nodo final correspondiente
	(mapcar	
		#'(lambda (action)
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
		
(defun expand-node (node problem)				; Devuelve una lista de nodos a los que transitar 
	(get_states node							;desde el nodo pasado por parametro, en base a los elementos del problema
		(list_actions node (problem-operators problem)) 
		(problem-f-h problem)))

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 6
;;
;;
;; Devuelve una lista de nodos ordenada en base a la estrategia
;; 
;;

(defun insert_node (node lst-nodes strategy)	; Inserta el nodo en la lista de nodos 
	(let ((f-lst (first lst-nodes)))			; en base a la estrategia
		(if lst-nodes
			(if (funcall strategy node f-lst)	; Si la estrategia devuelve t, devuelve el nodo 
				(cons node lst-nodes)			; concatenando con el resto de la lista
				(cons							
					f-lst						; Si la estrategia devuelve nil, 
					(insert_node				; sigue buscando la posicion del nodo
						node 
						(rest lst-nodes) 
						strategy)))
			(cons node nil))))					; Si al finalizar la lista no se ha encontrado la posicion, 
												; se concatena el nodo al final de la lista
												
(defun insert-nodes-strategy (nodes lst-nodes strategy)
	(if nodes
		(insert-nodes-strategy (rest nodes) 
			(insert_node 
				(first nodes) 
				lst-nodes 
				(strategy-node-compare-p strategy))
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
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 7
;;
;;
;; Parametro que define la estrategia A*
;; 
;;

(defun node-f-<= (node-1 node-2)   
	(<= (node-f node-1)				; Devuelve la comparacion de los valores de funcion
	(node-f node-2)))

(defparameter *A-star*  			; Define la estrategia A*
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
				((not (member node lst_close :test (problem-f-search-state-equal problem)))
					(graph-search-rec 
						(insert-nodes-strategy (expand-node node problem) (rest lst_open) strategy)
						(cons node lst_close)
						problem
						strategy))
				((g-node-compare node lst_close)
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
			:action nil
			:parent nil))
		nil
		problem
		strategy))
		
(defun a-star-search (problem)
    (graph-search problem *A-star*)) 
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 9
;;

;; Recorre desde el nodo solucion al nodo inicio
;; guardando los nombres de los nodos padres de manera inversa
;; para que salga ordenado
(defun path-recur-cola (node sol)
	(if node
		(path-recur-cola (node-parent node) (cons (node-state node) sol))
		sol))

;; Devuelve el path de inicio a fin
(defun solution-path (node)
	(path-recur-cola node nil))
	
;; Invierte la lista de acciones para que salgan
;; de inicio a fin
(defun action-recur-cola (node sol)
	(if node
		(action-recur-cola (node-parent node) (cons (node-action node) sol))
		sol))

;; Devuelve la lista de acciones de inicio a fin
(defun action-sequence (node)
	(action-recur-cola node nil)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 10
;;
;; Comparacion de nodos en busqueda en profundidad
(defun depth-first-node-compare-p (node-1 node-2) 
	(>= (node-depth node-1) (node-depth node-2))) 

;; Estrategia de busqueda en profundidad
(defparameter *depth-first*   
	(make-strategy  
		:name 'depth-first  
		:node-compare-p #'depth-first-node-compare-p))

;; Comparacion de nodos en busqueda en anchura
(defun breadth-first-node-compare-p (node-1 node-2) 
	(<= (node-depth node-1) (node-depth node-2))) 

;; Estrategia de busqueda en anchura
(defparameter *breadth-first*   
	(make-strategy  
		:name 'breadth-first  
		:node-compare-p #'breadth-first-node-compare-p)) 
		
;; Comparacion de nodos en coste uniforme
(defun uniform-cost-node-compare-p (node-1 node-2) 
	(< (node-g node-1) (node-g node-2))) 

;; Estrategia de busqueda por coste uniforme
(defparameter *uniform-cost*   
	(make-strategy  
		:name 'uniform-cost  
		:node-compare-p #'uniform-cost-node-compare-p)) 