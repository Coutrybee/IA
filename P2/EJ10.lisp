(defun depth-first-node-compare-p (node-1 node-2) 
	(< (node-depth node-1) (node-depth node-2))) 

(defparameter *depth-first*   
	(make-strategy  
		:name 'depth-first  
		:node-compare-p #'depth-first-node-compare-p))

(solution-path (graph-search *galaxy-M35* *depth-first*)) 


(defun breadth-first-node-compare-p (node-1 node-2) 
	(< (node-depth node-1) (node-depth node-2))) 

(defparameter *breadth-first*   
	(make-strategy  
		:name 'breadth-first  
		:node-compare-p #'breadth-first-node-compare-p)) 
 

 
(solution-path (graph-search *galaxy-M35* *breadth-first*)) 