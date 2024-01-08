grandchild(X, Y) :- child(X, Z), child(Z,Y).

child(tom, john).
child(ann, tom).
child(john, mark).
child(alice, john).

proud(X) :- parent(X, Y), newborn(Y).
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).
father(tom, ann) :- newborn(ann).

grandfather(x,z) :- father(x,y), parent(y,z).
father(nehru, indira).
mother(indira, rajiv).

natNumber(zero).
natNumber(succ(X)) :- natNumber(X).