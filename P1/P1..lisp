(defun recur (n sol)
	(if (< n 1)
		sol
		(recur (- n 1) (* sol n))))
(defun fact (n)
	(recur n 1))

(fact 5 )

(defun my-length (n)
	(if (listp  n)
		(recur n 0)
	nil))
(defun recur (n sol)
	(if (and  n '())
		sol
		(recur (cdr n) (+ sol 1))))

(setf nota nil)
(cond 
	((not nota) 'nil)
	((< nota 0) 'negativa)
	((<= nota 5) 'suspenso)
      ((<= nota 7) 'aprobado)
      ((<= nota 9) 'notable)
       (t 'sobresaliente))

(mapcar #'(lambda (x) (* x x)) '(1 2 3))
(setf sqr (lambda (x) (* x x)))
(mapcar sqr '(1 2 3))
(maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
(funcall sqr 3)
(apply sqr '(3))
(apply #'+ '(1 2 3 4))

(defun sum-range (n sol)
		(sum-range (- n 1) (+ sol n)))

(mapcar #'(suma (x y) (+ x y)) #'(lista (x) ((if (<= x 0) (cons x (lista (- x 1)))))))

(defun lista (x) ( 
	(if (<= x 0) 
		0 
		(cons x (lista (- x 1)) ) ) ) )

(reduce #'+ (lista 10))



(defun my-member (n l comp) 
	(if (funcall comp n (car l)) l (my-member n (rest l) comp)))

(defun my-count (n l) 
	(if (not (car (my-member n l 'eq) ))
		0
		(+ (my-count n (rest (my-member n l 'eq))) 1)))


(defun bisect (f a b tol)
	(if (> tol (- b a))
		(/ (+ a b) 2)
		'nil))

(defun bisect (f a b tol)
	(if (>= 0 (* (funcall f a) (funcall f b)))
		(if (> tol (funcall f (- b a)))
			(funcall f (/ (+ a b) 2))
			'noEncontrado)
		'noRaiz))

(let ( (mit '(/ (+ a b) 2)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;				Ejercicio 2
;;
;;
;;
;; Finds a root of f between the points a and b using bisection. 				
;; 
;; If f(a)f(b)>0 there is no guarantee that there will be a root in the 
;; interval, and the function will return NIL. 
;; 
;; f: function of a single real parameter with real values whose root 
;; we want to find 
;; a: lower extremum of the interval in which we search for the root 
;; b: b>a upper extremum of the interval in which we search for the root 
;; tol: tolerance for the stopping criterion: if b-a < tol the function 
;; returns (a+b)/2 as a solution. 
;;

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

(bisect #'(lambda(x) (sin (* 6.26 x))) 0.1 0.7 0.001) ;;---> 0.5020995
(bisect #'(lambda(x) (sin (* 6.26 x))) 0.0 0.7 0.001)
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 1.5 0.001)
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 2.1 0.001)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 
;; Finds all the roots that are located between consecutive values of a list 
;; of values 
;; 
;; Parameters: 
;; 
;; f: function of a single real parameter with real values whose root 
;; we want to find 
;; lst: ordered list of real values (lst[i] < lst[i+1]) 
;; tol: tolerance for the stopping criterion: if b-a < tol the function 
;; returns (a+b)/2 as a solution. 
;; 
;; Whenever sgn(f(lst[i])) != sgn(f(lst[i+1])) this function looks for a 
;; root in the corresponding interval. 
;; 
;; Returns: 
;; A list o real values containing the roots of the function in the 
;; given sub-intervals 
;;

(defun allroot (f lst tol)
	(if (null (car (rest lst)))
		nil
		(cons (bisect f (car lst) (car (rest lst)) tol) (allroot f (rest lst) tol))))

(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001) 

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; 
;; Divides an interval up to a specified length and find all the roots of 
;; the function f in the intervals thus obtained. 
;; 
;; Parameters: 
;; 
;; f: function of a single real parameter with real values whose root 
;; we want to find 
;; a: lower extremum of the interval in which we search for the root 
;; b: b>a upper extremum of the interval in which we search for the root 
;; N: Exponent of the number of intervals in which [a,b] is to be divided: 
;; [a,b] is divided into 2^N intervals 
;; tol: tolerance for the stopping criterion: if b-a < tol the function 
;; returns (a+b)/2 as a solution.
;; 
;; The interval (a,b) is divided in intervals (x[i], x[i+i]) with 
;; x[i]= a + i*dlt; a root is sought in each interval, and all the roots 
;; thus found are assembled into a list that is returned. 
;; 
;; 
;; Hint: 
;; One might find a way to use allroot to implement this function. This is 
;; possible, of course, but there is a simple way of doing it recursively 
;; without using allroot. 
;;

; Intervalos crea una lista de numeros de 'a' a 'b' de n en n
(defun intervalos (a b n) ;(/ (- b a) n) -> n
	(if (> b a)
		(cons a (intervalos (+ a n) b n))
		nil))

(defun allind (f a b N tol) 
	(allroot f (intervalos a b (/ (- b a) (expt 2 N))) tol))


(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 1 0.0001) 
(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 2 0.0001)


;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;				Ejercicio 3
;;

(defun combine-elt-elt (elt1 elt2)
	(if (or (null elt1) (null elt2))
		nil
		(cons elt1 (cons elt2 nil))))

(defun combine-elt-lst (elt lst)
	(if (null (car lst))
		nil
		(cons (cons elt (cons (car lst) nil)) (combine-elt-lst elt (rest lst)))))

(defun combine-elt-lst (elt lst)
	(if (null (car lst))
		nil
		(mapcar #'(lambda (x) (list elt x)) lst)))


(combine-elt-lst 'a nil)
(combine-elt-lst 'a '(1 2 3))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;

(defun combine-lst-lst (lst1 lst2) 
	(if (or (null (car lst1)) (null (car lst2)))
		nil
		(reduce'append (mapcar #'(lambda (x) (combine-elt-lst x lst2)) lst1))))





(combine-lst-lst nil nil) 			;; --> NIL 
(combine-lst-lst '(a b c) nil) 		;; --> NIL
(combine-lst-lst NIL '(a b c)) 		;; --> NIL 
(combine-lst-lst '(a b c) '(1 2 3)) 	;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;


(defun combine-list-of-lsts (lst) 
	(if (null (rest (rest lst)))
		(combine-lst-lst (car lst) (car (rest lst)))
		(combine-lst-lst (car lst) (combine-list-of-lsts (rest lst)))))





		
(defun combine-elts (lst)
	(if (listp (car lst))
		(append (combine-elts (car lst)) (rest lst))
		lst))


(defun combine-list (lst)
	(if (null (rest lst))
		(mapcar #'(lambda (x) (list x)) (car lst))
		(mapcar 'combine-elts (reduce 'combine-lst-lst lst))))

(defun combine-list-of-lsts (lst) 
	 (combine-list lst))


(combine-list-of-lsts '((1 2 3 4) (5 6 7) (8 9 10)))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4) (5 6 7)))
(combine-list-of-lsts '(()(a b c)))
(combine-list-of-lsts '((a) (+) (1) (5)))

(combine-list '((a b c) (+ -) (1 2 3 4)))
(combine-elts '(((a b) c) 4))