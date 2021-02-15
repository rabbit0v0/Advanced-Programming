% Advanced Programming Assignment 3
% Skeleton for warm-up part. Predicates to implement:

% add(N1, N2, N)
add(z,N2,N2).
add(s(N1),N2,s(Sum)):-add(N1,N2,Sum).


mult(z, N2, z).
mult(s(N1),N2,Res):- mult(N1,N2,Res1),add(Res1,N2,Res).


comp(z, z, eq).
comp(s(N1), z, gt).
comp(z, s(N2), lt).
comp(s(N1), s(N2), A):- comp(N1,N2,A).



% insert(N, TI, TO):-
insert(N, node(N1,L_Leaf,R_Leaf), node(N1,L_Leaf,R_Leaf)):- comp(N,N1,eq).
insert(N, node(N1,L_Leaf,R_Leaf), node(N1,TO,R_Leaf)):- comp(N,N1,lt),insert(N,L_Leaf,TO).
insert(N, node(N1,L_Leaf,R_Leaf), node(N1,L_Leaf,TO)):- comp(N,N1,gt),insert(N,R_Leaf,TO).
insert(N, leaf, node(N,leaf,leaf)).



% insertlist(Ns, TI, TO)
insertlist([], TI, TI).
insertlist([Head|Tail], TI,TO):- insert(Head,TI,TO1), insertlist(Tail,TO1,TO).

    
