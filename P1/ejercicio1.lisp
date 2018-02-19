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

(defun insert (vec cat lst)
	(cond ((eql vec nil) (append lst vec))
		((eql lst nil) (cons vec lst))
		(t (if (> (sc-rec cat vec) (sc-rec cat (first lst)))
				(cons vec lst)
				(cons (first lst) (insert vec cat (rest lst)))))))

(defun sc-conf-rec (cat vs conf)
	(if (null vs)
		nil
		(if (> (sc-rec cat (first vs)) conf)
			(insert (first vs) cat (sc-conf-rec cat (rest vs) conf))
			(sc-conf-rec cat (rest vs) conf))))

(defun sc-conf(cat vs conf)
	(if (and vs cat)
		(sc-conf-rec cat vs conf)
		nil))

;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 1.3 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun sc-classifier-aux (cats text func)
	(reduce 'max (mapcar #'(lambda (x) (funcall func (rest x) (rest text))) cats)))

(defun sc-classifier (cats texts func)
	(mapcar #' (lambda (x) (cons (first x) (sc-classifier-aux cats x func))) texts))