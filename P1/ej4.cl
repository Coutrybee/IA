;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion de simbolos que representan valores de verdad,
;; conectores y predicados para evaluar si una expresion LISP
;; es un valor de verdad o un conector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '~)

(defun truth-value-p (x) 
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x) 
  (eql x +not+))

(defun binary-connector-p (x) 
  (or (eql x +bicond+) 
      (eql x +cond+)))

(defun n-ary-connector-p (x) 
  (or (eql x +and+) 
      (eql x +or+)))

(defun connector-p (x) 
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))

(defun our-listp (x)
	(or (null x)
		(and (consp x)
			(our-listp (cdr x)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.1
;; Predicado para determinar si una expresion en LISP
;; es un literal positivo 
;;
;; RECIBE   : expresion 
;; EVALUA A : T si la expresion es un literal positivo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun positive-literal-p (x)
  (if (or (connector-p x) (truth-value-p x) (our-listp x))
		nil 
		T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.2
;; Predicado para determinar si una expresion
;; es un literal negativo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si la expresion es un literal negativo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun negative-literal-p (x)
	(and (our-listp x) (unary-connector-p (first x)) (positive-literal-p (second x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.3
;; Predicado para determinar si una expresion es un literal  
;;
;; RECIBE   : expresion x  
;; EVALUA A : T si la expresion es un literal, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun literal-p (x)
  (if (or (negative-literal-p x) (positive-literal-p x))
	T
	nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-prefix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato prefijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato prefijo
        (and (our-listp x)         ;; En caso de que no sea un literal debe ser una lista
             (let ((connector (first x))
                   (rest_1    (rest  x)))
               (cond
                ((unary-connector-p connector)  ;; Si el primer elemento es un connector unario
                 (and (null (rest rest_1))      ;; deberia tener la estructura (<conector> FBF)
                      (wff-prefix-p (first rest_1)))) 
                ((binary-connector-p connector) ;; Si el primer elemento es un conector binario
                 (let ((rest_2 (rest rest_1)))  ;; deberia tener la estructura 
                   (and (null (rest rest_2))    ;; (<conector> FBF1 FBF2)
                        (wff-prefix-p (first rest_1))
                        (wff-prefix-p (first rest_2)))))               
                ((n-ary-connector-p connector)  ;; Si el primer elemento es un conector enario
                 (or (null rest_1)              ;; conjuncion o disyuncion vacias
                     (and (wff-prefix-p (first rest_1)) ;; tienen que ser FBF los operandos 
                          (let ((rest_2 (rest rest_1)))
                            (or (null rest_2)           ;; conjuncion o disyuncion con un elemento
                                (wff-prefix-p (cons connector rest_2)))))))	
                (t NIL)))))))                 ;; No es FBF en formato prefijo 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.4
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun wff-infix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato prefijo (por convencion)
    (or (literal-p x)         ;; Un literal es FBF en formato prefijo
        (and (our-listp x)         ;; En caso de que no sea un literal debe ser una lista
             (or (let* ((literal_1 (first x))
				   (connector (second x))
				   (literal_2 (third x))
                   (rest_1    (rest  x))
				   (rest_2 (rest rest_1))
					(rest_3 (rest rest_2)))
					(cond
						((unary-connector-p literal_1)  ;; Si el primer elemento es un connector unario
						 (and (null (rest rest_1))      ;; deberia tener la estructura (<conector> FBF)
						      (wff-infix-p connector))) 
						((binary-connector-p connector) ;; Si el primer elemento es un conector binario
						   (and (null (rest rest_2))    ;; (<conector> FBF1 FBF2)
							(wff-infix-p literal_1)
							(wff-infix-p literal_2)))               
						((n-ary-connector-p connector)  ;; Si el primer elemento es un conector enario
						     (if (null rest_3)
								(and (wff-infix-p literal_1) (wff-infix-p literal_2))
								(and (wff-infix-p literal_1) (wff-infix-p rest_1))))
						((and (n-ary-connector-p literal_1) (null rest_1)) T)	
						(t NIL)))	
				(let* ((connector_1 (first x))
				   (connector_2 (third x))
                   (rest_1    (rest  x)))
					(cond              
						((n-ary-connector-p connector_1)  ;; Si el primer elemento es un conector enario
							(when (equal connector_2 connector_1)
								(wff-infix-p rest_1)))
						(t NIL))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convierte FBF en formato prefijo a FBF en formato infijo
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF en formato infijo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-to-infix (wff)
  (when (wff-prefix-p wff)
    (if (literal-p wff)
        wff
      (let ((connector      (first wff))
            (elements-wff (rest wff)))
        (cond
         ((unary-connector-p connector) 						;; Si el primer elemento es un connector unario
          (list connector (prefix-to-infix (second wff))))
         ((binary-connector-p connector) 						;; Si el primer elemento es un conector binario
          (list (prefix-to-infix (second wff))
                connector
                (prefix-to-infix (third wff))))
         ((n-ary-connector-p connector) 						;; Si el primer elemento es un conector enario
          (cond 
           ((null elements-wff)        							;; conjuncion o disyuncion vacias. 
            wff)                       							;; por convencion, se acepta como fbf en formato infijo
           ((null (cdr elements-wff))  							;; conjuncion o disyuncion con un unico elemento
            (prefix-to-infix (car elements-wff)))  
           (t (cons (prefix-to-infix (first elements-wff)) 
                    (mapcan #'(lambda(x) (list connector (prefix-to-infix x))) ;; Concatenar primer elemento, conector y el resto de la lista transformada
                      (rest elements-wff))))))
         (t NIL)))))) ;; no deberia llegar a este paso nunca

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.5
;;
;; Convierte FBF en formato infijo a FBF en formato prefijo
;;  
;; RECIBE   : FBF en formato infijo 
;; EVALUA A : FBF en formato prefijo 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infix-to-prefix (wff)
  (when (wff-infix-p wff)
    (if (literal-p wff)
        wff
      (let* ((elemento_1      (first wff))
			(conector      (second wff))
			(elemento_2      (third wff))
            (elements-wff (rest wff)))
        (cond
         ((unary-connector-p elemento_1) 						;; Si el primer elemento es un connector unario
          (list elemento_1 (infix-to-prefix conector)))

         ((binary-connector-p conector) 						;; Si el primer elemento es un conector binario
          (list conector
				(infix-to-prefix elemento_1)
                (infix-to-prefix elemento_2)))

		 ((n-ary-connector-p elemento_1) 						;; Si el primer elemento es un conector enario
          (cond 
           ((null elements-wff)        							;; Condicion de parada
            wff)))                       
           
         ((n-ary-connector-p conector) 							;; Si el segundo elemento es conector, cambiamos a prefix por cada elemento
          (infix_n-ary_prefix wff))
         (t NIL)))))) ;; no deberia llegar a este paso nunca

(defun infix_n-ary_prefix (wff)
	(cons 	(second  wff) 
			(mapcar #'(lambda(x) ( infix-to-prefix x)) 
                      (recur wff))))

(defun recur (wff)
	(when wff
	(cons (car wff) (recur (rest (rest wff))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.6
;; Predicado para determinar si una FBF es una clausula  
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : T si FBF es una clausula, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-p (wff)
	(when (our-listp wff)
	  (when (and (eql (car wff) +or+) (lista-literales-clause-p (rest wff)))
			t)))

;; Devuelve los literales de un FBF
(defun lista-literales-clause-p (wff)
	(if (null  wff) 
		t
		(when (literal-p (car wff))
			(lista-literales-clause-p (rest wff)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1.7
;; Predicado para determinar si una FBF esta en FNC  
;;
;; RECIBE   : FFB en formato prefijo 
;; EVALUA A : T si FBF esta en FNC con conectores, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnf-p (wff)
  	(when (our-listp wff)
	  (when (and (eql (car wff) +and+) (lista-literales-cnf-p (rest wff)))
			t)))

;;Devuelve un lista de clausulas sin los conectores que lo unen
(defun lista-literales-cnf-p (wff)
	(if (null  wff) 
		t
		(when (clause-p (car wff))
			(lista-literales-cnf-p (rest wff)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.1: Incluya comentarios en el codigo adjunto
;;
;; Dada una FBF, evalua a una FBF equivalente 
;; que no contiene el connector <=>
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF equivalente en formato prefijo 
;;            sin connector <=>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-biconditional (wff)
  (if (or (null wff) (literal-p wff))							;; si wff es un literal o nil, devuelve wff
      wff
    (let ((connector (first wff)))
      (if (eq connector +bicond+)								;; Transforma bicondicional en and de cond de sus elementos
          (let ((wff1 (eliminate-biconditional (second wff)))
                (wff2 (eliminate-biconditional (third  wff))))
            (list +and+ 
                  (list +cond+ wff1 wff2)
                  (list +cond+ wff2 wff1)))
        (cons connector 										;; Enlista el conector seguido de el resto de expresiones
              (mapcar #'eliminate-biconditional (rest wff)))))));; transformando los bicondicionales

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.2
;; Dada una FBF, que contiene conectores => evalua a
;; una FBF equivalente que no contiene el connector =>
;;
;; RECIBE   : wff en formato prefijo sin el connector <=> 
;; EVALUA A : wff equivalente en formato prefijo 
;;            sin el connector =>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-conditional (wff)  
   (if (or (null wff) (literal-p wff))							;; si wff es un literal o nil, devuelve wff
      wff
    (let ((connector (first wff)))
      (if (eq connector +cond+)								;; Transforma el condicional en su formula con or y not
          (let ((wff1 (eliminate-conditional (second wff)))
                (wff2 (eliminate-conditional (third  wff))))
            (list +or+ 
                  (list +not+ wff1)
                  wff2 ))
        (cons connector 										;; Enlista el conector seguido de el resto de expresiones
              (mapcar #'eliminate-conditional (rest wff)))))))	;; transformando los bicondicionales

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.3
;; Dada una FBF, que no contiene los conectores <=>, => 
;; evalua a una FNF equivalente en la que la negacion  
;; aparece unicamente en literales negativos
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, => 
;; EVALUA A : FBF equivalente en formato prefijo en la que 
;;            la negacion  aparece unicamente en literales 
;;            negativos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduce-scope-of-negation (wff)
  (if (or (null wff) (literal-p wff) (n-ary-connector-p wff))
		wff
		(cond 
			((eql (car wff) +not+)
				(negar_lista (reduce-scope-of-negation (second wff))))
			(t
				(cons (reduce-scope-of-negation (car wff)) (reduce-scope-of-negation (rest wff)))))))

;;Niega los elementos de una clausula, sin el conector previo
(defun negar_literales (x)
	(when x
		(if (literal-p (car x))
			(cons (negar_literal (car x)) (negar_literales (rest x)))
			(cons (negar_lista (car x)) (negar_literales (rest x))))))

;;Niega una clausula
(defun negar_lista (wff)
		(when wff
			(if (literal-p wff)
				(negar_literal wff)
				(cons (exchange-and-or (car wff)) (negar_literales (rest wff))))))

;;Niega un literal
(defun negar_literal (x)
	(cond 
		((negative-literal-p  x)
			(second x))
		((positive-literal-p x)
			(list +not+ x))
		(t nil)))

;;Intercambia or por and y vicevrsa
(defun exchange-and-or (connector)
  (cond
   ((eq connector +and+) +or+)    
   ((eq connector +or+) +and+)
   (t connector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.4: Comente el codigo adjunto 
;;
;; Dada una FBF, que no contiene los conectores <=>, => en la 
;; que la negacion aparece unicamente en literales negativos
;; evalua a una FNC equivalente en FNC con conectores ^, v  
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, =>, 
;;            en la que la negacion aparece unicamente 
;;            en literales negativos
;; EVALUA A : FBF equivalente en formato prefijo FNC 
;;            con conectores ^, v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Combina el elemento elt con cada elemento de la lista lst
(defun combine-elt-lst (elt lst)
  (if (null lst)
      (list (list elt))
    (mapcar #'(lambda (x) (cons elt x)) lst)))

;;Aplica la ley de asociatividad a los elementos en disyuncion
(defun exchange-NF (nf)
  (if (or (null nf) (literal-p nf)) 
      nf
    (let ((connector (first nf)))
      (cons (exchange-and-or connector) ;; intercambia el conector and por or y viceversa
            (mapcar #'(lambda (x)
                          (cons connector x))
                (exchange-NF-aux (rest nf))))))) 

;;Combina listas de literales haciendo un producto cartesiano
(defun exchange-NF-aux (nf)
  (if (null nf) 
      NIL
    (let ((lst (first nf)))
      (mapcan #'(lambda (x) 
                  (combine-elt-lst 
                   x 
                   (exchange-NF-aux (rest nf)))) 
        (if (literal-p lst) (list lst) (rest lst))))))


;; Combina los wffs o literales que tienen el mismo conector, poniendolos al mismo nivel
(defun simplify (connector lst-wffs )
  (if (literal-p lst-wffs)
      lst-wffs                    
    (mapcan #'(lambda (x) 
                (cond 
                 ((literal-p x) (list x))
                 ((equal connector (first x))
                  (mapcan 
                      #'(lambda (y) (simplify connector (list y))) 
                    (rest x))) 
                 (t (list x))))               
      lst-wffs)))

;;Transforma una lista en prefijo a FNC
(defun cnf (wff)
  (cond
   ((cnf-p wff) wff)
   ((literal-p wff)
    (list +and+ (list +or+ wff))) 											;;Por convencion
   ((let ((connector (first wff))) 
      (cond
       ((equal +and+ connector) 
        (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff)))))			;;En caso de estar en conjuncion pone todo al mismo nivel
       ((equal +or+ connector) 												;;En caso de estar en disyuncion lo transforma a conjuncion 
        (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff)))))))))))    ;;y aplica la ley de asociatividad 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.5:
;;
;; Dada una FBF en  FNC
;; evalua a lista de listas sin conectores
;; que representa una conjuncion de disyunciones de literales
;;
;; RECIBE   : FBF en FNC con conectores ^, v
;; EVALUA A : FBF en FNC (con conectores ^, v eliminaos)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-connectors (cnf)
	(when cnf
	  (cond 
		((n-ary-connector-p (car cnf))						;; Si es un conector enario
			(eliminate-connectors (rest cnf)))				;; Recursivamente elimina el resto de conectores
		((our-listp (car cnf))									;; Si es una lista
			(cons (eliminate-connectors (car cnf)) (eliminate-connectors (rest cnf)))) ;; Concatena el primer elemento sin conectores con el resto de la lista 
		(t (cons (car cnf) (eliminate-connectors (rest cnf)))))))		;; En otro caso concatena el elemento con el resto de la lista sin conectores

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.6
;; Dada una FBF en formato infijo
;; evalua a lista de listas sin conectores
;; que representa la FNC equivalente
;;
;; RECIBE   : FBF 
;; EVALUA A : FBF en FNC (con conectores ^, v eliminados)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-to-cnf (wff)
	(eliminate-connectors (cnf (reduce-scope-of-negation (eliminate-conditional (eliminate-biconditional (infix-to-prefix wff))))))) ;; Transforma a prefijo y aplica todos los pasos																																	 ;; para transformar un FBF a FNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.1
;; eliminacion de literales repetidos una clausula 
;; 
;; RECIBE   : K - clausula (lista de literales, disyuncion implicita)
;; EVALUA A : clausula equivalente sin literales repetidos 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-repeated-literals (k)
  (when k
		(if (comprueba-rep (car k) (rest k))			;; Si el primero esta contenido en el reto de la lista
			(eliminate-repeated-literals (rest  k))		;; Ignoramos el elemento repetido y seguimos analizando
			(cons (car k) (eliminate-repeated-literals (rest k))))))	;; Concatemanos el primer elemento para que no se pierda

;; Comprueba si un elemento esta contenido en la lista
(defun comprueba-rep (elt lst)
	(when lst
		(if (equal elt (car lst))
			T
			(comprueba-rep elt (rest lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.2
;; eliminacion de clausulas repetidas en una FNC 
;; 
;; RECIBE   : cnf - FBF en FNC (lista de clausulas, conjuncion implicita)
;; EVALUA A : FNC equivalente sin clausulas repetidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-repeated-clauses (cnf) 
  (when cnf
		(if (comprueba-rep-clause (car cnf) (rest cnf)) 
			(eliminate-repeated-clauses (rest  cnf))
			(cons (eliminate-repeated-literals (car cnf)) (eliminate-repeated-clauses (rest cnf))))))

;; Comprueba si cl1 y cl2 son iguales en elementos, no importa el orden
(defun repited-clause (cl1 cl2)
	(let ((el1 (eliminate-repeated-literals cl1))
			(el2 (eliminate-repeated-literals cl2)))		
		(and 
			(clause-in-clause el1 el2) (clause-in-clause el2 el1))))

;; Comprueba si cl1 esta contenido en cl2
(defun clause-in-clause ( cl1 cl2)
	(or (null cl1) 
		(and (comprueba-rep (car cl1) cl2)
			(clause-in-clause (rest cl1) cl2))))

;; Comprueba si una clausula esta en la lista
(defun comprueba-rep-clause (cl1 lst)	
	(when lst
		(if (repited-clause cl1 (car lst))
			T
			(comprueba-rep-clause cl1 (rest lst)))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;m
;; EJERCICIO 4.3.3
;; Predicado que determina si una clausula subsume otra
;;
;; RECIBE   : K1, K2 clausulas
;; EVALUA a : K1 si K1 subsume a K2
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subsume (K1 K2)
  (when (clause-in-clause k1 k2)		;; Si K1 esta contenido en K2
	(list k1)))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.4
;; eliminacion de clausulas subsumidas en una FNC 
;; 
;; RECIBE   : cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-subsumed-clauses (cnf)
	(if (equal cnf '(nil))
		cnf
	(mimap (car cnf) cnf cnf)))

(defun mimap (elt lst lst2)
	(when lst2
		(let ((comprueba (comprueba-subs elt lst)))
			(if comprueba
				(cons comprueba (mimap (second lst2) lst (rest lst2)))
			(mimap (second lst2) lst (rest lst2))))))

(defun comprueba-subs (cl1 lst)	
	(if lst
		(cond
			((repited-clause cl1 (car lst))
				(comprueba-subs cl1 (rest lst)))
			((subsume (car lst) cl1)
				nil)
			(t (comprueba-subs cl1 (rest lst))))
		cl1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.5
;; Predicado que determina si una clausula es tautologia
;;
;; RECIBE   : K (clausula)
;; EVALUA a : T si K es tautologia
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tautology-p (K) 
    (when k
		(if (comprueba-rep (negar_literal(car k)) (rest k))		;; Si el negado de un elemento esta contenido en el resto de la lista
			t
			(tautology-p (rest k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.6
;; eliminacion de clausulas en una FBF en FNC que son tautologia
;;
;; RECIBE   : cnf - FBF en FNC
;; EVALUA A : FBF en FNC equivalente a cnf sin tautologias 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-tautologies (cnf) 
	  (when cnf
		(if (tautology-p (car cnf))									;; Si el primer elemento contiene tautologias
			(eliminate-tautologies (rest cnf))
			(cons (car cnf) (eliminate-tautologies (rest cnf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.7
;; simplifica FBF en FNC 
;;        * elimina literales repetidos en cada una de las clausulas 
;;        * elimina clausulas repetidas
;;        * elimina tautologias
;;        * elimina clausulass subsumidas
;;  
;; RECIBE   : cnf  FBF en FNC
;; EVALUA A : FNC equivalente sin clausulas repetidas, 
;;            sin literales repetidos en las clausulas
;;            y sin clausulas subsumidas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify-cnf (cnf) 
  (when cnf
		(eliminate-tautologies (eliminate-subsumed-clauses (eliminate-repeated-clauses cnf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; Construye el conjunto de clausulas lambda-neutras para una FNC 
;;
;; RECIBE   : cnf    - FBF en FBF simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(0) subconjunto de clausulas de cnf  
;;            que no contienen el literal lambda ni ~lambda   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-neutral-clauses (lambda cnf) 
  (when cnf
		(if (or (comprueba-rep lambda (car cnf)) (comprueba-rep (negar_literal lambda) (car cnf)))		;; Si el elemento esta contenido en positivo o negativo en la primera clausula de cnf
			(extract-neutral-clauses lambda (rest cnf))
			(cons (car cnf) (extract-neutral-clauses lambda (rest cnf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.2
;; Construye el conjunto de clausulas lambda-positivas para una FNC
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(+) subconjunto de clausulas de cnf 
;;            que contienen el literal lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-positive-clauses (lambda cnf) 
  (when cnf
		(if (comprueba-rep lambda (car cnf))			;; Si el elemento esta contenido en la primera clausula de la lista
			(cons (car cnf) (extract-positive-clauses lambda (rest cnf)))
			(extract-positive-clauses lambda (rest cnf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; Construye el conjunto de clausulas lambda-negativas para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo 
;; EVALUA A : cnf_lambda^(-) subconjunto de clausulas de cnf  
;;            que contienen el literal ~lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-negative-clauses (lambda cnf) 
  (when cnf
		(if (comprueba-rep (negar_literal lambda) (car cnf))				;; Si el elemento negado esta contenido en la primera clausula de la lista
			(cons (car cnf) (extract-negative-clauses lambda (rest cnf)))
			(extract-negative-clauses lambda (rest cnf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; resolvente de dos clausulas
;;
;; RECIBE   : lambda      - literal positivo
;;            K1, K2      - clausulas simplificadas
;; EVALUA A : res_lambda(K1,K2) 
;;                        - lista que contiene la 
;;                          clausula que resulta de aplicar resolucion 
;;                          sobre K1 y K2, con los literales repetidos 
;;                          eliminados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun resolve-on (lambda K1 K2) 
	(when (and lambda k1 k2)
		(when 
			(or (and (extract-positive-clauses lambda (list k1)) 
						(extract-negative-clauses lambda (list k2)))
					(and (extract-positive-clauses lambda (list k2)) 
						(extract-negative-clauses lambda (list k1))))
				(extract-literals lambda k1 k2))))

(defun extract-literals (lambda k1 k2)
	(list (extract-literal lambda (union k1 k2))))

(defun extract-literal (elt lst)
	(when lst
		(if (or (equal elt (car lst)) (equal (negar_literal elt) (car lst)))
			(extract-literal elt (rest lst))
			(cons (car lst) (extract-literal elt (rest lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; Construye el conjunto de clausulas RES para una FNC 
;;
;; RECIBE   : lambda - literal positivo
;;            cnf    - FBF en FNC simplificada
;;            
;; EVALUA A : RES_lambda(cnf) con las clauses repetidas eliminadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-RES (lambda cnf)
	( eliminate-repeated-clauses (append (res-aux lambda 
		(extract-positive-clauses lambda cnf) (extract-negative-clauses  lambda cnf)) (extract-neutral-clauses  lambda cnf))))

;Itera sobre la lista de clausulas positivas y, para cada una de ellas, resuelve sobre todas las negativas
(defun res-aux (lambda positive negative)
	(when (car positive)
		(append (res-map-aux lambda (car positive) negative) (res-aux lambda (rest positive) negative))))

;Aplica la funcion resolve-on sobre lambda y una clausula positiva para toda la lista de clausulas negativas
(defun res-map-aux (lambda ele-p negative)
	(mapcan #'(lambda (x) (resolve-on lambda ele-p x))
		negative))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.5
;; Comprueba si una FNC es SAT calculando RES para todos los
;; atomos en la FNC 
;;
;; RECIBE   : cnf - FBF en FNC simplificada
;; EVALUA A :	T  si cnf es SAT
;;                NIL  si cnf es UNSAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun  RES-SAT-p (cnf) 
	(pivote cnf (extr cnf)))

;Resuelve sobre cnf para cada literal de literals
(defun pivote (cnf literals)
	(cond
		((null cnf)				;Si llega a tautologia, es SAT
			t)
		((equal cnf '(nil))		;Si llega a contradiccion, es UNSAT
			nil)
		((null literals)		;Si ha acabado de resolver sobre todos los literales y no es nil, es SAT
			t)
		(t (when literals		;Itera y simplifica
			(pivote (simplify-cnf (build-RES (car literals) cnf)) (rest literals))))))

;transforma un lista de literales a lista de literales positivos
(defun to-positive (lst)
	(when lst
		(if (negative-literal-p (first lst))
			(cons (negar_literal (first lst)) (to-positive (rest lst)))
			(cons (first lst) (to-positive (rest lst))))))

;Extrae la lista de literales positivos (sin repetidos) que componen el cnf
(defun extr (cnf)
	(eliminate-repeated-literals (to-positive (extr-rec cnf))))

;Extrae la lista de literales que componen el cnf
(defun extr-rec (cnf)
	(when cnf
		(union (car cnf) (extr-rec (rest cnf)) :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.6:
;; Resolucion basada en RES-SAT-p
;;
;; RECIBE   : wff - FBF en formato infijo 
;;            w   - FBF en formato infijo 
;;                               
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logical-consequence-RES-SAT-p (wff w)
	(if (RES-SAT-p (union (wff-infix-to-cnf wff) (wff-infix-to-cnf (negar_literal w))))	
		nil
		t))