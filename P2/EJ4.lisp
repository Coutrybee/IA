;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 4
;;
(defparameter *galaxy-M35*
    (make-problem
        :states *planets*
        :initial-state *planet-origin*
        :f-goal-test #'(lambda (node)
                (f-goal-test-galaxy node *planets-destination* 
                    *planets-mandatory*))
        :f-search-state-equal #'(lambda (node-1 node-2)
                (f-search-state-equal-galaxy node-1 node-2 *planets-mandatory*)) 
        :f-h #'(lambda (state) (f-h-galaxy state *sensors*))
        :operators (list  
                        #'(lambda (state)
                            (navigate-worm-hole state *worm-holes* *planets-forbidden*))
                        #'(lambda (state)
                            (navigate-white-hole state *white-holes*))))) ;;Van las funciones de navigate
 