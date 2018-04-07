? elem_count(b,[b,a,b,a,b],Xn). 
 Xn = 3, false. 
 
?- elem_count(a,[b,a,b,a,b],Xn). 
Xn = 2, false. 
 
 elem_count(_,[],0).
 elem_count(A,[A|Rs],Xn) :-
    elem_count(A,Rs,C),
    Xn is C +1.
 elem_count(A,[B|Rs],Xn) :-
    B \= A,
    elem_count(A,Rs,Xn).