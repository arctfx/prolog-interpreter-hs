sum(N, zero, N).
sum(N, s(M), s(K)) :- sum(N, M, K).

nat(zero).
nat(succ(X)) :- nat(X).