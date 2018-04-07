;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 8
;;
;;
;; Devuelve el nodo terminal que es solucion o nil si no hay solucion
;; 
;;

;; Busca el nodo en la lista y, si lo encuentra,
;; devuelve la comparacion segun la estrategia.
;; Si no lo encuentra devuelve t
(defun node-compare (node lst strategy)
	(if lst
		(if (equal 
				(node-state node) 
				(node-state (first lst))) 
			(funcall strategy node (first lst))					
			(node-compare node (rest lst) strategy))
		t))

(defun graph-search-rec (lst_open lst_close problem strategy)
	(when lst_open														; Si no quedan nodos en la lista de abiertos, devuelve nil
		(let ((node (first lst_open)))
			(cond 
				((funcall (problem-f-goal-test problem) node)			; Comprueba si es nodo final
					node)
				((or													; Si el nodo no esta en la lista de cerrados 
						(member 										; o tiene un coste menor que el insertado
							node										; se inserta en la lista de cerrados y 
							lst_close									; se insertan los nodos resultantes de su expansion
							:test-not									; en la lista de abiertos segun la estrategia
								(problem-f-search-state-equal problem)) 
						(node-compare node lst_close 
							(strategy-node-compare-p strategy)))
					(graph-search-rec 
						(insert-nodes-strategy 
							(expand-node node problem) 
							(rest lst_open) 
							strategy)
						(cons node lst_close)
						problem
						strategy))
				(t (graph-search-rec									; En cualquier otro caso se elimina de la lista de abiertos
							(rest lst_open)								; y continua la busqueda
							lst_close
							problem
							strategy))))))

(defun graph-search (problem strategy) 
	(graph-search-rec 
		(list (make-node							; Crea el primer nodo con los valores por defecto
			:state (problem-initial-state problem) 
			:depth 0 
			:action nil
			:parent nil
			:g 0 
			:h (f-h-galaxy 
					(problem-initial-state problem) 
					*sensors*)
			:f (f-h-galaxy 
					(problem-initial-state problem) 
					*sensors*) ))
		nil
		problem
		strategy))

;;;;
;;
;; Devuelve el camino solucion desde el nodo generado con graph-search
;; 
;; 
 
(defun a-star-search (problem)
    (graph-search problem *A-star*)) 
		
(graph-search *galaxy-M35* *A-star*)
(a-star-search *galaxy-M35*)

