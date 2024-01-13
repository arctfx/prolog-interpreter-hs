sum(N, zero, N).
sum(N, s(M), s(K)) :- sum(N, M, K).
parent(pesho, gosho).
parent(gosho, ivan).
parent(ivan, penka).
parent(penka, asen).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
ancestor(X, X).