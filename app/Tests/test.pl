parent(pesho, gosho).
parent(gosho, ivan).
parent(ivan, penka).
parent(penka, asen).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
ancestor(X, X).
sd
vvdfdv dsv 
 

 asencs
 dcg_translate_rule(In, Out)
 