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
		
(defparameter node-01    (make-node :state 'Avalon :depth 0 :g 0 :f 0) ) 
(defparameter node-02    (make-node :state 'Kentares :depth 2 :g 50 :f 50 :parent node-01) ) 
(defparameter node-03    (make-node :state 'Katril :parent node-02 :depth 3 :g 150 :f 2))
(defparameter node-04    (make-node :state 'Katril :parent node-03 :depth 3 :g 200 :f 5))
(insert-nodes-strategy (list node-01 node-02) (list node-03 node-04) *A-star*)