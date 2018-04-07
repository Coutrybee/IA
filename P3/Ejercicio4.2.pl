?- list_count([b],[b,a,b,a,b],Xn). 
Xn = [b-3] False 
 
?- list_count([b,a],[b,a,b,a,b],Xn). 
Xn = [b-3, a-2] False 
 
?- list_count([b,a,c],[b,a,b,a,b],Xn). 
Xn = [b-3, a-2, c-0] false

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :-
    concatena(L1, L2, L3).

 elem_count(_,[],0).
 elem_count(A,[A|Rs],Xn) :-
    elem_count(A,Rs,C),
    Xn is C +1.
 elem_count(A,[B|Rs],Xn) :-
    B \= A,
    elem_count(A,Rs,Xn).

list_count([],_,[]).
list_count([A|Rs],B,Xn) :-
    elem_count(A,B,C),
    list_count(Rs,B,D),
    concatena([A-C],D,Xn).
