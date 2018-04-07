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
		
(defparameter node-01    
	(make-node :state 'Avalon :depth 0 :g 0 :f 0) ) 
(defparameter node-02    
	(make-node :state 'Kentares :depth 2 :g 50 :f 50) ) 
 
(print (insert-nodes-strategy 
	(list node-00 node-01 node-02)
		lst-nodes-00                          
		*uniform-cost*))
		
		
(defparameter node-01    (make-node :state 'Avalon :depth 0 :g 0 :f 0) ) 
(defparameter node-02    (make-node :state 'Kentares :depth 2 :g 50 :f 50 :parent node-01) ) 
(defparameter node-03    (make-node :state 'Katril :parent node-02 :depth 3 :g 150 :f 2))
(defparameter node-04    (make-node :state 'Katril :parent node-03 :depth 3 :g 200 :f 5))
(insert-nodes-strategy (list node-01 node-02) (list node-03 node-04) *uniform-cost*)


;;;(#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0) 
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20) 
;;; #S(NODE :STATE AVALON :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20) 
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 12) :DEPTH 13 :G 22 :H 5 :F 27) 
;;; #S(NODE :STATE DAVION :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20) 
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 14) :DEPTH 13 :G 24 :H 1 :F 25) 
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20) 
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 17) :DEPTH 13 :G 27 :H 7 :F 34) 
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20) 
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 10) :DEPTH 13 :G 20 :H 0 :F 20) 
;;; #S(NODE :STATE KENTARES :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20) 
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 21) :DEPTH 13 :G 31 :H 4 :F 35) 
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20) 
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 16) :DEPTH 13 :G 26 :H 7 :F 33) 
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20) 
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 7) :DEPTH 13 :G 17 :H 0 :F 17) 
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50))

(insert-nodes-strategy '(4 8 6 2) '(1 3 5 7)   
    (make-strategy  :name 'simple      :node-compare-p #'<)) 
;-> (1 2 3 4 5 6 7)