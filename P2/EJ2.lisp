(defun navigate (name state net planets-forbidden)
    (when net
        (cond ((member (second (car net)) planets-forbidden)
                (navigate name state (rest net) planets-forbidden))
                ((equal state (caar net))
                    (cons (make-action :name name :origin state :final (second (car net)) :cost (third (car net))) (navigate name state (rest net) planets-forbidden)))
                (t (navigate name state (rest net) planets-forbidden)))))


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