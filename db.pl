:- use_module(library(lists)).

mem(_, []) :- fail.
mem(X, [X|_]).
mem(X, [_|R]) :- mem(X, R).

mem(X, union(A, _)) :- mem(X, A).
mem(X, union(_, B)) :- mem(X, B).

mem(X, inter(A, B)) :- mem(X, A), mem(X, B).

mem((X, Y), cartesian(A, B)) :- mem(X, A), mem(Y, B).

subs([], _).
subs([X|L], R) :- mem(X, R), subs(L, R).

eqset(L, R) :- subs(L, R), subs(R, L).


