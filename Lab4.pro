%-- Sidiropoulos Georgios 4488 --
%-----------------------------------------------------------------------------------------

%-- ASKHSH 1

...(0,M,K,[]).
...(1,M,K,[M]).
...(N,M,K,L) :- NE is N-1,V is M+NE*K,helper(NE,M,K,[V],L),!.
 
conc([],L,L).
conc([H|T],L,[H|Q]):-conc(T,L,Q).

helper(N,M,K,L,Z):-N>1,NE is N-1,V is M+NE*K,conc([V],L,T),helper(NE,M,K,T,Z).
helper(1,M,K,L,Z):-conc([M],L,T),Z = T.

%-----------------------------------------------------------------------------------------

%-- ASKHSH 2

common(_,[],[]).
common([],_,[]).
common(X,Y,L) :-helper2(Y,X,[],1,Z),L = Z,!.

helper2([Y|Y2],[X|X2],L,N,Z):-Y=X,conc(L,[N],K),E is N+1,helper2(Y2,X2,K,E,Z).
helper2([Y|Y2],[X|X2],L,N,Z):-Y\=X,E is N+1,helper2(Y2,X2,L,E,Z).
helper2(_,_,[],N,Z):-Z = [].
helper2(_,[],L,N,Z):-Z = L.
helper2([],_,L,N,Z):-Z = L.

%-----------------------------------------------------------------------------------------

%-- ASKHSH 3

freq(L,S) :- helper3(L,L,[],[],S),!.

counter(_,[],0).
counter(X,[X|T],N):-!,counter(X,T,N1),N is N1+1.
counter(X,[_|T],N):-counter(X,T,N). 

helper3([],L,V,M,Z):- Z = M.
helper3([H|T],L,V,M,Z):-checker(H,V,C),C==0,checker(H,L,C1),AD=C1*H,conc(M,[AD],M1),conc(V,[H],V1),helper3(T,L,V1,M1,Z).
helper3([H|T],L,V,M,Z):-checker(H,V,C),C>0,helper3(T,L,V,M,Z).

checker(H,Y,Z):-counter(H,Y,Z).

%-----------------------------------------------------------------------------------------

%-- ASKHSH 4

count(L,C):-loop2(L,L,Z),C is Z.

loop1([],0,_).
loop1([H|T],N,C):-H>2*C,loop1(T,N1,C),N is N1+1.
loop1([H|T],N,C):-loop1(T,N1,C),N is N1+0.

loop2([],Z,N):-N is 0,!.
loop2([H|T],Z,N):-loop1(Z,C,H),loop2(T,Z,X),N is X+C,!.






