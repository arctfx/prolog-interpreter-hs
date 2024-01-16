and(A,B) :- A, B.

or(A,B) :- A.
or(A,B) :- B.

equ(A,B) :- or(and(A,B), and(not(A),not(B))).

xor(A,B) :- not(equ(A,B)).

nor(A,B) :- not(or(A,B)).

nand(A,B) :- not(and(A,B)).

impl(A,B) :- or(not(A),B).

bind(true).
bind(fail).