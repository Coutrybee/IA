;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1
;;
;;
;; Devuelve el valor de heuristica de un estado 
;; desde Mallory
;;
;;

(defun f-h-galaxy (state sensors)
	(when sensors
		(let ((sensor-first (first sensors)))
			(if (equal state (first sensor-first))
				(second sensor-first)
				(f-h-galaxy state (rest sensors))))))


(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth *sensors*) ;-> NIL