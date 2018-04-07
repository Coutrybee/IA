;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 4
;;
;;
;; Define la galaxia m35 como un problema
;; 
;;

(defparameter *galaxy-M35*
    (make-problem
        :states *planets*
        :initial-state *planet-origin*
        :f-goal-test 
            #'(lambda (node)
                (f-goal-test-galaxy node 
                    *planets-destination* 
                    *planets-mandatory*))
        :f-search-state-equal 
            #'(lambda (node-1 node-2)
                (f-search-state-equal-galaxy 
                    node-1 
                    node-2 
                    *planets-mandatory*)) 
        :f-h #'(lambda (state) 
                (f-h-galaxy state *sensors*))
        :operators 
            (list  
                #'(lambda (state)
                    (navigate-worm-hole 
                        state 
                        *worm-holes* 
                        *planets-forbidden*))
                #'(lambda (state)
                    (navigate-white-hole 
                        state 
                        *white-holes*))))) 
 
 
 
 (defparameter *mygalaxy*
    (make-problem
        :states '(Davion Mallory Katril)
        :initial-state *planet-origin*
        :f-goal-test #'(lambda (node)
                (f-goal-test-galaxy node *planets-destination* 
                    '(Davion)))
        :f-search-state-equal #'(lambda (node-1 node-2)
                (f-search-state-equal-galaxy node-1 node-2 '(Davion))) 
        :f-h #'(lambda (state) (f-h-galaxy state *sensors*))
        :operators (list  
                        #'(lambda (state)
                            (navigate-worm-hole state *worm-holes* *planets-forbidden*))
                        #'(lambda (state)
                            (navigate-white-hole state *white-holes*))))) ;;Van las funciones de navigate
 