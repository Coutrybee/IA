;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1
;;
(defun f-h-galaxy (state sensors)
	(when sensors
		(if (equal state (first (first sensors)))
			(second (first sensors))
			(f-h-galaxy state (rest sensors)))))


(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth *sensors*) ;-> NIL