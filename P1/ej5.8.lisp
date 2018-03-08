(defun bfs (end queue net) 
	(if (null queue) 
		'() 
		(let* ((path (first queue)) 
				(node (first path))) 
			(if (eql node end) 
				(reverse path) 
				(bfs end 
					(append (rest queue) 
							(new-paths path node net)) 
					net))))) 

(defun bfs (end queue net) 
	(if (null queue) 
		'() 
		(let* ((path (first queue)) 
				(node (first path))
				(next_nodes (new-paths path node net))) 
			(if (eql node end) 
				(reverse path) 
				(bfs end 
					(append (rest queue) 
							(repe_list next_nodes (rest queue))) 
					net))))) 
(defun new-paths (path node net) 
	(mapcar #'(lambda(n) 
			(cons n path)) 
		(rest (assoc node net))))

(defun shortest-path (start end net) 
	(unless (repe_elt_net end net)
		(bfs end (list (list start)) net)))

(defun repe_elt_net (elt lst2)
	(when lst2
		(if (repe elt (car lst2)) 
			t
			(repe_elt_net elt (rest lst2)))))

(defun repe(elt lst)
	(when lst 
		(if (equal elt (car lst))
			t
			(repe elt (rest lst)))))


(defun repe_list (elts lst)
	(when elts
		(if (repe (car elts) lst) 
			t
			(repe_list (rest elts) lst))))

(trace bfs)
(trace new-paths)
(trace repe_elt_net)

(repe_elt_net 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
(repe_elt_net 'e '((a b c) (b a c d) (c a b d) (d a b c )))

(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))

(shortest-path 'f 'c '((a b c d e) (b a d e f) (c a g) (d a b g h) (e a b g h) (f b h) (g c d e h) (h d e g f)))

(shortest-path 'a 'e '((a b c) (b a c d) (c a b d) (d a b c )))