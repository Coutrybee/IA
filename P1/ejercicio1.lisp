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

