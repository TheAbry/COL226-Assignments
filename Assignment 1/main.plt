:- begin_tests(lists).
:- use_module(library(lists)).

/* TESTCASES FOR PART A */ 

test(test_aa1, [fail]) :- mem((2,3), reftransclos([(3,2)],[1,2,3,4,5])).
test(test_aa2) :- mem((2,4), reftransclos([(2,3), (3,4)],[1,2,3,4,5])).
test(test_aa3) :- mem((1,1), reftransclos([],[1,2,3,4,5])).
test(test_aa4, [fail]) :- mem((1,1), reftransclos([],[2,3,4,5])).
test(test_aa5) :- mem((5,3), reftransclos([(2,5), (5, 1), (1, 3)],[1,2,3,4,5])).
test(test_aa6, [fail]) :- mem((3,5), reftransclos([(2,5), (5, 1), (1, 3)],[1,2,3,4,5])).
test(test_aa7) :- mem((1,2), reftransclos([(1,4),(1,3),(3,2),(3,1)],[1,2,3,4,5])).
test(test_aa8, [fail]) :- mem((1,2), reftransclos([(1,3),(2,3)],[1,2,3])).
test(test_aa9, [fail]) :- mem((3,2), reftransclos([(1,2),(2,4),(3,3)],[1,2,3,4])).

test(test_ab1) :- mem((2,3), eqclos([(3,2)],[1,2,3,4,5])).
test(test_ab2) :- mem((2,4), eqclos([(2,3), (3,4)],[1,2,3,4,5])).
test(test_ab3) :- mem((1,1), eqclos([],[1,2,3,4,5])).
test(test_ab4, [fail]) :- mem((1,1), eqclos([],[2,3,4,5])).
test(test_ab5) :- mem((5,3), eqclos([(2,5), (5, 1), (1, 3)],[1,2,3,4,5])).
test(test_ab6) :- mem((3,5), eqclos([(2,5), (5, 1), (1, 3)],[1,2,3,4,5])).
test(test_ab7) :- mem((1,2), eqclos([(1,4),(1,3),(3,2),(3,1)],[1,2,3,4,5])).
test(test_ab8) :- mem((1,2), eqclos([(1,3),(2,3)],[1,2,3])).
test(test_ab9, [fail]) :- mem((3,2), eqclos([(1,2),(2,4),(3,3)],[1,2,3,4])).

/* TESTCASES FOR PART B */

test(test_ba1) :- interI([], [], X), X=[].
test(test_ba2) :- interI([], [1,2], X), X=[].
test(test_ba3) :- interI([2,3,4], [], X), X=[].
test(test_ba4) :- interI([2,3,4], [1,2], X), X=[2].
test(test_ba5) :- interI([5,2,1,3], [3,1,4], X), X=[1,3].

test(test_bb1) :- diffI([], [], X), X=[].
test(test_bb2) :- diffI([2,3,5], [], X), X=[2,3,5].
test(test_bb3) :- diffI([], [1,7,6], X), X=[].
test(test_bb4) :- diffI([1,2,3,4], [1,2,3,4], X), X=[].
test(test_bb5) :- diffI([1,2,3,4], [5,6], X), X=[1,2,3,4].
test(test_bb6) :- diffI([1,3,4,5], [1,2,3], X), X=[4,5].

test(test_bc1) :- cartesianI([], [1,2], X), X=[].
test(test_bc2) :- cartesianI([1,2], [], X), X=[].
test(test_bc3) :- cartesianI([1], [1,2], X), X=[(1,1), (1,2)].
test(test_bc4) :- cartesianI([2,3], [1,4], X), X=[(2,1), (2,4), (3,1), (3,4)].
test(test_bc5) :- cartesianI([1,2,3,4], [1], X), X=[(1,1), (2,1), (3,1), (4,1)].

:- end_tests(lists).
