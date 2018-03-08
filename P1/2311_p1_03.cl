;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1.1a ;;
;;;;;;;;;;;;;;;;;;;;

;;Realiza el cuadrado de un elemento
(defun cuadrado (x)
	(* x x))

;;Realiza el sumatorio de los elementos de la lista al cuadrado de forma recursiva
(defun sumatorio-sqr_rec (lista)
	(if lista
		(eval (+ (cuadrado (first lista)) (sumatorio-sqr_rec (rest lista))))
		0 ))

;;Realiza el sumatorio de los elementos de 2 listas de forma recursiva
(defun sumatorio-listas_rec (lista1 lista2)
	(if lista1
		(eval (+ (* (first lista1) (first lista2)) (sumatorio-listas_rec (rest lista1) (rest lista2))))
		0))

;;Realiza el coseno entre 2 vectores de forma recursiva
(defun sc-rec (x y)
	(/ (sumatorio-listas_rec x y) (* (sqrt (sumatorio-sqr_rec x)) (sqrt (sumatorio-sqr_rec y)))))

;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1.1b ;;
;;;;;;;;;;;;;;;;;;;;

;;Realiza el sumatorio de los elementos de la lista al cuadrado con mapcar
(defun sumatorio-sqr_mapcar (lista)
	(apply '+ (mapcar #'(lambda (x) (+ (cuadrado x))) lista)))

;;Realiza el sumatorio de los elementos de 2 listas con mapcar
(defun sumatorio-listas_mapcar (lista1 lista2)
	(apply '+ (mapcar #'(lambda (x y) (* x y)) lista1 lista2)))

;;Realiza el coseno entre 2 vectores usando mapcar
(defun sc-mapcar (x y)
	(/ (sumatorio-listas_mapcar x y) (* (sqrt (sumatorio-sqr_mapcar x)) (sqrt (sumatorio-sqr_mapcar y)))))

;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1.2 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun insert (vec cat lst)
	(cond
		((eql vec nil) 				;;Si el vector esta vacio se inserta al final
			(append lst vec))
		((eql lst nil) 				;;Si hemos acabado de iterar sobre la lista devolvemos el vector en una lista
			(cons vec lst))
		(t (if (> (sc-rec cat vec) (sc-rec cat (first lst)))		;;Comparamos similitud del vector con la lista e iteramos
				(cons vec lst)										;;Si es mayor lo insertamos antes
				(cons (first lst) (insert vec cat (rest lst)))))))	;;Si es menor, comparamos con el siguiente vector

(defun sc-conf(cat vs conf)
	(when vs
		(if (> (sc-rec cat (first vs)) conf)
			(insert (first vs) cat (sc-conf cat (rest vs) conf))	;;Si la similitud es mayor que el conf lo insertamos
			(sc-conf cat (rest vs) conf))))							;;Si es menr, analizamos con el siguiente

;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1.3 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun sc-classifier-aux (cats text func)
	(reduce 'max (mapcar #'(lambda (x) (funcall func (rest x) (rest text))) cats)))

(defun sc-classifier (cats texts func)
	(mapcar #' (lambda (x) (cons (first x) (sc-classifier-aux cats x func))) texts))

;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 2.1 ;;;
;;;;;;;;;;;;;;;;;;;;

; Calcula la raiz de un intervalo, dada una tolerancia
(defun bisect (f a b tol)
	(if (>= (sgn f a b) 0)
		nil
		(if (> tol (absol (- (funcall f b) (funcall f a))))
			(mit a b)
			(if (> 0 (sgn f a (mit a b)))
				(bisect f a (mit a b) tol)
				(if (> 0 (sgn f (mit a b) b))
					(bisect f (mit a b) b tol)
					'nil )))))

(defun sgn (f a b )
	(* (funcall f a) (funcall f b)))

(defun mit (a b)
	(/ (+ a b) 2))

(defun absol (a)
	(sqrt (* a a)))


;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 2.2 ;;;
;;;;;;;;;;;;;;;;;;;;

; Calcula las raices de los intervalos dados consecutivamente en una lista
(defun allroot (f lst tol)
	(if (null (car (rest lst)))
		nil
		(cons (bisect f (car lst) (car (rest lst)) tol) (allroot f (rest lst) tol))))


;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 2.3 ;;;
;;;;;;;;;;;;;;;;;;;;

; Intervalos crea una lista de numeros de 'a' a 'b' de n en n
(defun intervalos (a b n)
	(if (> b a)
		(cons a (intervalos (+ a n) b n))
		nil))

; Calcula las raices de un intervalo, separado en intervalos de tamaño n, con una tolerancia dada
(defun allind (f a b N tol)
	(allroot f (intervalos a b (/ (- b a) (expt 2 N))) tol))


;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3.1 ;;;
;;;;;;;;;;;;;;;;;;;;

; Combina el elemento 'elt' con cada uno de los elementos de la lista
(defun combine-elt-lst (elt lst)
	(mapcar #'(lambda (x) (list elt x)) lst))

;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3.2 ;;;
;;;;;;;;;;;;;;;;;;;;

; Combina dos listas haciendo un producto cartesiano
(defun combine-lst-lst (lst1 lst2)
	(if (or (null (car lst1)) (null (car lst2)))
		nil
		(reduce'append (mapcar #'(lambda (x) (combine-elt-lst x lst2)) lst1))))


;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3.3 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun our-listp (x)
	(or (null x)
		(and (consp x)
			(our-listp (cdr x)))))

; Une los elementos que sean una lista, de la lista 'lst', en una sola lista
(defun combine-elts (lst)
	(if (our-listp (car lst))
		(append (combine-elts (car lst)) (rest lst))
		lst))

; Combina las listas de la lista 'lst' haciendo un producto cartesiano
(defun combine-list-of-lsts (lst)
	(if (null (rest lst))
	 (mapcar #'(lambda (x) (list x)) (car lst))
	 (mapcar 'combine-elts (reduce 'combine-lst-lst lst))))
