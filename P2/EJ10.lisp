;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(solution-path (graph-search *galaxy-M35* *depth-first*)) 
(solution-path (graph-search *galaxy-M35* *breadth-first*)) 
(solution-path (graph-search *galaxy-M35* *A-star*))