?-sort_list([p-0, a-6, g-7, t-2], X). 
X = [p-0, t-2, a-6, g-7] false 
 
?-sort_list([p-0, a-6, g-7, p-9, t-2], X). 
X = [p-0, t-2, a-6, g-7, p-9] false 
 
?-sort_list([p-0, a-6, g-7, p-9, t-2, 9-99], X).
X = [p-0, t-2, a-6, g-7, p-9, 9-99] false 
 

sort_list([],[]).
sort_list([A|Rs1],R) :-
	sort_list(Rs1,Ri),
    insert([A],Ri,R).
 
 
insert([Z-X],[], [Z-X]).
insert([Z-X],[B-A|Rs], R) :-
    A > X,
    Rs \= [],
    concatena([Z-X],[B-A|Rs],R).
insert([Z-X],[B-A|[]], [Z-X,B-A]) :-
    A > X.
    
insert([Z-X],[B-A|Rs],[B-A|R]) :-
    A =< X,
    insert([Z-X],Rs, R).
    
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :-
    concatena(L1, L2, L3).