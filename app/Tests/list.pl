append(empty, L, L).
append(cons(H, T1), L2, cons(H, T3)) :- append(T1, L2, T3).