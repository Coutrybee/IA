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
	(Mallory Proserpina 12) (Mallory Katril 5) (Mallory Avalon 9))) 



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