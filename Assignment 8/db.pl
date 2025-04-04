ismale(bob).
haschild(bob).
haschild(alice).

isdad(X) :- ismale(X), haschild(X).
