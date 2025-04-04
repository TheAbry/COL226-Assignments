/* I have assumed the existence of a print function in the language.
The print function has return type unit.*/

:- use_module(library(lists)).
mem(_, []) :- fail.
mem(X, [X|_]) :- !.
mem(X, [_|R]) :- mem(X, R).

subs([], _) :- !.
subs(_, []) :- fail.
subs([X|L], [X|R]) :- subs(L, R), !.
subs([X|L], R) :- mem(X, R), subs(L, R).

mem(true, boolD).
mem(false, boolD).

nmem(X, S) :- \+ mem(X, S), !.

hastype(_, num(N), intT) :- integer(N), !.
hastype(_, bl(B), boolT) :- mem(B, boolD), !.

hastype(S, var(X), intT) :- nmem((X, boolT), G), mem((X, intT), G), subs(S, G).
hastype(S, var(X), boolT) :- nmem((X, intT), G), mem((X, boolT), G), subs(S, G).
hastype(_, print(_), unitT).

hastype(G, plus(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, sub(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, times(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, div(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, pow(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).

hastype(G, eq(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, gt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, geq(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, lt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, leq(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).

hastype(G, or(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, nor(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, xor(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, and(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, nand(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).

hastype(G, not(E1), boolT) :- hastype(G, E1, boolT).


