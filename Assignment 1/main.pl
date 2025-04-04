:- use_module(library(lists)).

/* STARTER CODE */

app([], L, L).
app([X|R], L, [X|Z]) :- app(R, L, Z).

mycons(_, [], []) :- !.
mycons(X, [Y|R], [(X,Y)|S]) :- mycons(X, R, S).

/* HELPER FUNCTIONS */
mem(_, []) :- fail.
mem(X, [X|_]) :- !.
mem(X, [_|R]) :- mem(X, R).
mem((X,Y), reftransclos(R,S)) :- mem((X,Y), reftransclos(R,S), []). 

/* PART A: SPECIFICATION */

mem((X,X), reftransclos(_,S), _) :- mem(X, S),!.
mem((X,Y), reftransclos(R,_), _) :- mem((X,Y), R),!.
mem((X,Z), reftransclos(R,S), A) :- mem((X,Y), R), \+mem(Y, A), mem((Y,Z), reftransclos(R,S), [X|A]),!.
mem((X,Z), reftransclos(R,S), A) :- mem((Y,Z), R), \+mem(Y, A), mem((X,Y), reftransclos(R,S), [X|A]).

mem((X,X), eqclos(_,S)) :- mem(X, S), !.
mem((X,Y), eqclos(R,S)) :- mem((X,Y), reftransclos(R,S)), !.
mem((X,Y), eqclos(R,S)) :- mem((Y,X), reftransclos(R,S)), !.

/* BONUS SPECIFICATION 

mem((X,Y), symclos(R)) :- mem((X,Y), R), !.
mem((X,Y), symclos(R)) :- mem((Y,X), R).

mem((X,Y), eq_clos(R,S)) :- mem((X,Y), symclos(reftransclos(R,S))). */

/* PART B: IMPLEMENTATION */

interI([], _, []) :- !.
interI([X|S1], S2, [X|S]) :- mem(X, S2), interI(S1, S2, S), !.
interI([X|S1], S2, S) :- \+ mem(X, S2), interI(S1, S2, S).

diffI([], _, []) :- !.
diffI([X|S1], S2, [X|S]) :- \+ mem(X, S2), diffI(S1, S2, S), !.
diffI([X|S1], S2, S) :- mem(X, S2), del(X, S2, S3), diffI(S1, S3, S).

cartesianI([], _, []) :- !.
cartesianI([X|S1], S2, S) :- mycons(X, S2, S3), cartesianI(S1, S2, S4), app(S3, S4, S).

/* Given two power sets, we can find the largest subset of this power set by defining a length predicate.
Now, after obtaining the two largest subsets of the power sets, suppose S1 and S2, we can check diff(S1, S2, []), diff(S2, S1, []).
This is sufficient to say whether the two power sets are equal, assuming of course that they are valid power sets. */

/*
unionI([1,2], [], X), X=[1,2].
unionI([], [1,2], X), X=[1,2].
unionI([1,2], [2,1], X), X=[1,2].
unionI([1,2,3], [3], X), X=[1,2,3].
unionI([1,2], [3], X), X=[1,2,3].
*/

/* TESTCASES FOR PART A */ 
/*
mem((2,3), reftransclos([(3,2)],[1,2,3,4,5])).
mem((2,4), reftransclos([(2,3), (3,4)],[1,2,3,4,5])).
mem((1,1), reftransclos([],[1,2,3,4,5])).
mem((1,1), reftransclos([],[2,3,4,5])).
mem((5,3), reftransclos([(2,5), (5, 1), (1, 3)],[1,2,3,4,5])).
mem((3,5), reftransclos([(2,5), (5, 1), (1, 3)],[1,2,3,4,5])).
mem((1,2), reftransclos([(1,4),(1,3),(3,2),(3,1)],[1,2,3,4,5])).
mem((1,2), reftransclos([(1,3),(2,3)],[1,2,3])).
mem((3,2), reftransclos([(1,2),(2,4),(3,3)],[1,2,3,4])).
mem((1,6), reftransclos([(1,2), (2,3), (3,1), (2,4), (4,2), (4,6)],[1,2,3,4,5,6])).

mem((2,3), eqclos([(3,2)],[1,2,3,4,5])).
mem((2,4), eqclos([(2,3), (3,4)],[1,2,3,4,5])).
mem((1,1), eqclos([],[1,2,3,4,5])).
mem((1,1), eqclos([],[2,3,4,5])).
mem((5,3), eqclos([(2,5), (5, 1), (1, 3)],[1,2,3,4,5])).
mem((3,5), eqclos([(2,5), (5, 1), (1, 3)],[1,2,3,4,5])).
mem((1,2), eqclos([(1,4),(1,3),(3,2),(3,1)],[1,2,3,4,5])).
mem((1,2), eqclos([(1,3),(2,3)],[1,2,3])).
mem((3,2), eqclos([(1,2),(2,4),(3,3)],[1,2,3,4])).
mem((1,6), eqclos([(1,2), (2,3), (3,1), (2,4), (4,2), (4,6)],[1,2,3,4,5,6])).
*/
/* TESTCASES FOR PART B */
/*
interI([], [], X), X=[].
interI([], [1,2], X), X=[].
interI([2,3,4], [], X), X=[].
interI([2,3,4], [1,2], X), X=[2].
interI([5,2,1,3], [3,1,4], X), X=[1,3].

diffI([], [], X), X=[].
diffI([2,3,5], [], X), X=[2,3,5].
diffI([], [1,7,6], X), X=[].
diffI([1,2,3,4], [1,2,3,4], X), X=[].
diffI([1,2,3,4], [5,6], X), X=[1,2,3,4].
diffI([1,3,4,5], [1,2,3], X), X=[4,5].

cartesianI([], [1,2], X), X=[].
cartesianI([1,2], [], X), X=[].
cartesianI([1], [1,2], X), X=[(1,1), (1,2)].
cartesianI([2,3], [1,4], X), X=[(2,1), (2,4), (3,1), (3,4)].
cartesianI([1,2,3,4], [1], X), X=[(1,1), (2,1), (3,1), (4,1)].
*/