;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1.1a ;;
;;;;;;;;;;;;;;;;;;;;

(defun cuadrado (x)
	(* x x))

(defun sumatorio-sqr (lista)
	(if lista
		(eval (+ (cuadrado (first lista)) (sumatorio-sqr (rest lista))))
		0 ))

(defun sumatorio-listas (lista1 lista2)
	(if lista1
		(eval (+ (* (first lista1) (first lista2)) (sumatorio-listas (rest lista1) (rest lista2))))
		0))

(defun sc-rec (x y)
	(/ (sumatorio-listas x y) (* (sqrt (sumatorio-sqr x)) (sqrt (sumatorio-sqr y)))))

;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1.1b ;;
;;;;;;;;;;;;;;;;;;;;

(defun sumatorio-sqr (lista)
	(apply '+ (mapcar #'(lambda (x) (+ (cuadrado x))) lista)))

(defun sumatorio-listas (lista1 lista2)
	(apply '+ (mapcar #'(lambda (x y) (* x y)) lista1 lista2)))

(defun sc-mapcar (x y)
	(/ (sumatorio-listas x y) (* (sqrt (sumatorio-sqr x)) (sqrt (sumatorio-sqr y)))))

;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1.2 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun sc-conf(cat vs conf)
	(if vs
		(sort (copy-list(remove-if #'(lambda (y) (if (< (sc-rec cat y) conf) t)) vs)) #' (lambda (w z) (> (sc-rec cat w) (sc-rec cat z))))
		nil))

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

(defun allroot (f lst tol)
	(if (null (car (rest lst)))
		nil
		(cons (bisect f (car lst) (car (rest lst)) tol) (allroot f (rest lst) tol))))


;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 2.3 ;;;
;;;;;;;;;;;;;;;;;;;;

; Intervalos crea una lista de numeros de 'a' a 'b' de n en n
(defun intervalos (a b n) ;(/ (- b a) n) -> n
	(if (> b a)
		(cons a (intervalos (+ a n) b n))
		nil))

(defun allind (f a b N tol)
	(allroot f (intervalos a b (/ (- b a) (expt 2 N))) tol))


;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3.1 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun combine-elt-lst (elt lst)
	(if (null (car lst))
		nil
		(mapcar #'(lambda (x) (list elt x)) lst)))


;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3.2 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lst (lst1 lst2)
	(if (or (null (car lst1)) (null (car lst2)))
		nil
		(reduce'append (mapcar #'(lambda (x) (combine-elt-lst x lst2)) lst1))))


;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3.3 ;;;
;;;;;;;;;;;;;;;;;;;;


(defun combine-elts (lst)
	(if (listp (car lst))
		(append (combine-elts (car lst)) (rest lst))
		lst))

(defun combine-list-of-lsts (lst)
	(if (null (rest lst))
	 (mapcar #'(lambda (x) (list x)) (car lst))
	 (mapcar 'combine-elts (reduce 'combine-lst-lst lst))))
