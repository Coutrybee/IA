concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :-
    concatena(L1, L2, L3).

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