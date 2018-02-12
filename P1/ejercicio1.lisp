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

