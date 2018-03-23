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


(defparameter node-00    (make-node :state 'Proserpina :depth 12 :g 10 :f 20) ) 
 
(defparameter lst-nodes-00   (expand-node node-00 *galaxy-M35*))  
 
 
(print lst-nodes-00)

(expand-node (make-node :state 'Kentares :depth 0 :g 0 :f 0) *galaxy-M35*) 


;;;(#S(NODE :STATE AVALON 
;;;         :PARENT #S(NODE :STATE KENTARES 
;;;                         :PARENT NIL 
;;;                         :ACTION NIL 
;;;                         :DEPTH 0 
;;;                         :G ...) 
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE  
;;;                           :ORIGIN KENTARES  
;;;                           :FINAL AVALON  
;;;                           :COST 3) 
;;;         :DEPTH 1  
;;;         :G ...)  
;;; #S(NODE :STATE KATRIL  
;;;         :PARENT #S(NODE :STATE KENTARES  
;;;                         :PARENT NIL  
;;;                         :ACTION NIL  
;;;                         :DEPTH 0  
;;;                         :G ...)  
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE  
;;;                           :ORIGIN KENTARES  
;;;                           :FINAL KATRIL  
;;;                           :COST 10)  
;;;         :DEPTH 1  
;;;         :G ...)  
;;; #S(NODE :STATE PROSERPINA  
;;;         :PARENT #S(NODE :STATE KENTARES  
;;;                         :PARENT NIL  
;;;                         :ACTION NIL  
;;;                         :DEPTH 0  
;;;                         :G ...)  
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE  
;;;                           :ORIGIN KENTARES  
;;;                           :FINAL PROSERPINA  
;;;                           :COST 7)  
;;;         :DEPTH 1  
;;;         :G ...)  
;;; #S(NODE :STATE PROSERPINA  
;;;         :PARENT #S(NODE :STATE KENTARES  
;;;                         :PARENT NIL  
;;;                         :ACTION NIL  
;;;                         :DEPTH 0  
;;;                         :G ...)  
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE  
;;;                           :ORIGIN KENTARES  
;;;                           :FINAL PROSERPINA  
;;;                           :COST 12)  
;;;         :DEPTH 1  
;;;         :G ...)) 