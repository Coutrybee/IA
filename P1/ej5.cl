

;;Porque el recorrido en anchura en un grafo dirigido asegura encontrar una de las soluciones optimas.
(defun shortest-path (start end net)
(bfs end (list (list start)) net))

(defun bfs (end queue net)
	(if (null queue) 
		'()										;;Si la lista sobre la que itero se acaba, devuelvo NIL
		(let* ((path (first queue))
			(node (first path)))
		(if (eql node end)						;;Si el nodo que voy a analizar ahora es el final devuelvo el camino
			(reverse path)
			(bfs end
				(append (rest queue)
						(new-paths path node net)) net)))))	;;Si no, exploro las conexiones que tiene el nodo y sigo recorriendo el grafo

;;Devuelve una lista con las conexiones de un nodo en el grafo
(defun new-paths (path node net)
	(mapcar #'(lambda(n) (cons n path))
			(rest (assoc node net))))

(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
