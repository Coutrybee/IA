;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 2
;;
;;
;; Devuelve el conjunto de acciones desde un estado
;; 
;;

(defun navigate (name state net planets-forbidden)
    (let ((primero (first net))
            (resto (rest net)))
        (when net                                                       ; Si no queda ningun nodo devuelvo nil
            (cond 
                ((member (second primero) planets-forbidden)            ; Si el estado destino esta prohibido no hago nada
                    (navigate name state resto planets-forbidden))
                ((equal state (first primero))                          ; Si el el estado origen es el mismo que el dado creo 
                    (cons (make-action                                  ; un nodo y lo concateno con la salida recursiva
                            :name name 
                            :origin state 
                            :final (second primero) 
                            :cost (third primero)) 
                        (navigate name state resto planets-forbidden)))
                (t (navigate name state resto planets-forbidden))))))   ; En otro caso sigo buscando


(defun navigate-white-hole (state white-holes )
    (navigate 'NAVIGATE-WHITE-HOLE state white-holes nil))


(defun navigate-worm-hole (state worm-holes planets-forbidden)
    (navigate 'NAVIGATE-WORM-HOLE state worm-holes planets-forbidden))



(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))
(navigate-worm-hole 'Mallory *worm-holes* NIL) ;->
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL AVALON :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))
(navigate-white-hole 'Kentares *white-holes*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL PROSERPINA :COST 7))
(navigate-worm-hole 'Urano *worm-holes* *planets-forbidden*) ;-> NIL

'(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)