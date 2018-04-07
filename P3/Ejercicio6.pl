?-build_tree([p-0, a-6, g-7, p-9, t-2, 9-99], X).
X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil), tree(1, tree(p, nil, nil), tree(1, tree(t, nil, nil), tree(9, nil, nil)))))) 
false 
 
?-build_tree([p-55, a-6, g-7, p-9, t-2, 9-99], X).
X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil), tree(1, tree(p, nil, nil), tree(1, tree(t, nil, nil), tree(9, nil, nil)))))) 
False 
 
?-build_tree([p-55, a-6, g-2, p-1], X). 
X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil), tree(p, nil, nil)))) 
False 
 
?-build_tree([a-11, b-6, c-2, d-1], X). 
X = tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))