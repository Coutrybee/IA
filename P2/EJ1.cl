
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

(defun f-h-galaxy (state sensors)
	(when sensors
		(if (equal state (caar sensors))
			(second (car sensors))
			(f-h-galaxy state (rest sensors)))))


(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth *sensors*) ;-> NIL