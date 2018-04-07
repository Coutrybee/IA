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

(f-goal-test-galaxy-recur node-04 '(Avalon Katril))