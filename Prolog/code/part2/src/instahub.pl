% Advanced Programming Assignment 3
% Skeleton for main part. Predicates to implement:

%%% level 0 %%%

inside(X, [H|T]) :- inside_(T, X, H).
inside_(_, E, E).
inside_([H|T], E, _) :- inside_(T, E, H).

getName([], []).
getName([person(Name, _)|R], [Name|N]) :- getName(R, N).

getRest(X, [Head|Tail], Rest) :- getRest_(Tail, Head, X, Rest).

getRest_(Tail, Head, Head, Tail).
getRest_([Head2|Tail], Head, X, [Head|Rest]) :- getRest_(Tail, Head2, X, Rest).

different(G, X, Y) :-
getName(G, N),
inside(X, N),
inside(Y, N),
getRest(X, N, R),
inside(Y, R).

notInside(G, X, []).

notInside(G, X, [H|T]) :- notInside_(G, T, X, H).

notInside_(G, _, E, E) :- inside(E, []).

notInside_(G, [], E, Y) :- different(G, E, Y).

notInside_(G, [H|T], E, Y) :-
different(G, E, Y),
notInside_(G, T, E, H).

follows(G, X, Y) :- 
inside(person(X, Z), G),
inside(Y, Z).

ignores(G, X, Y) :-
inside(person(Y, Z), G),
inside(X, Z),
inside(person(X, A), G),
notInside(G, Y, A).

%%% level 1 %%%
checkPop(G, X, []).

checkPop(G, X, [Y|Z]) :-
follows(G, Y, X),
checkPop(G, X, Z).

popular(G, X) :-
inside(person(X, Z), G),
checkPop(G, X, Z).

checkOut(G, X, []).

checkOut(G, X, [Y|Z]) :-
ignores(G, Y, X),
checkOut(G, X, Z).

outcast(G, X) :-
inside(person(X, Z), G),
checkOut(G, X, Z).

%%% checkFollow: find people who follows X %%%
checkFollow(G, X, [], []).

checkFollow(G, X, [X|T], R) :-
checkFollow(G, X, T, R).

checkFollow(G, X, [H|T], [H|R]) :-
different(G, X, H),
follows(G, H, X),
checkFollow(G, X, T, R).

checkFollow(G, X, [H|T], R) :-
different(G, X, H),
inside(person(H, Z), G),
notInside(G, X, Z),
checkFollow(G, X, T, R).

%%% checkFriendly: all people in X are in Y %%%

checkFriendly([], _).

checkFriendly([X|T], Y) :-
inside(X, Y),
checkFriendly(T, Y).

friendly(G, X) :-
getName(G, N),
checkFollow(G, X, N, R),
inside(person(X, Z), G),
checkFriendly(R, Z).

checkHostile(G, [], _).

checkHostile(G, [X|T], Y) :-
notInside(G, X, Y),
checkHostile(G, T, Y).

hostile(G, X) :-
getName(G, N),
checkFollow(G, X, N, R),
inside(person(X, Z), G),
checkHostile(G, R, Z).

%%% level 2 %%%

addToTail([], L, L).
addToTail(L, [], L).
addToTail([H|T], L, [H|R]) :- addToTail(T, L, R).

checkAware(G, X, Y) :-
inside(person(X, Z), G),
inside(Y, Z).

checkUnaware(G, X, Y) :-
inside(person(X, Z), G),
notInside(G, Y, Z).

bfs(G, [X|T], Visited, Y) :-
inside(X, Visited),
bfs(G, T, Visited, Y).

bfs(G, [X|T], Visited, Y) :-
notInside(G, X, Visited),
checkAware(G, X, Y).

bfs(G, [X|T], Visited, Y) :-
notInside(G, X, Visited),
checkUnaware(G, X, Y),
addToTail([X], Visited, V),
inside(person(X, Z), G),
notInList(G, Z, V, R),
addToTail(R, T, TT),
bfs(G, TT, V, Y).

aware(G, X, Y) :-
different(G, X, Y),
bfs(G, [X], [], Y).

aware(_, X, X) :- inside(X, []).

%%% notInList:  elements in X but not in Y %%%
notInList(G, X, [], X).

notInList(G, [], _, []).

notInList(G, [X|T], Y, R) :-
inside(X, Y),
notInList(G, T, Y, R).

notInList(G, [X|T], Y, [X|R]) :-
notInside(G, X, Y),
notInList(G, T, Y, R).


awareList(_, [], _, []).

awareList(G, [X|T], Visited, L) :-
inside(X, Visited),
awareList(G, T, Visited, L).

awareList(G, [X|T], Visited, [X|L]) :-
notInside(G, X, Visited),
addToTail([X], Visited, V),
inside(person(X, Z), G),
notInList(G, Z, V, R),
addToTail(R, T, TT),
awareList(G, TT, V, L).

awareList1(G, [X|T], Visited, L) :-
notInside(G, X, Visited),
addToTail([X], Visited, V),
inside(person(X, Z), G),
addToTail(Z, T, TT),
awareList(G, TT, V, L).

ignorant(G, X, Y) :-
different(G, X, Y),
awareList1(G, [X], [], L),
notInside(G, Y, L).

%%% level 3 %%%

subReplace([], K, []).

subReplace([X|T], K, [Y|R]) :-
inside(p(X, Y), K),
subReplace(T, K, R).

replace([], K, []).

replace([person(X, L)|T], K, [person(Y, Z)|R]) :-
inside(p(X, Y), K),
subReplace(L, K, Z),
replace(T, K, R).

sameList([X1|L1], L2) :-
inside(X1, L2),
sameList(L1, L2).

sameList([], _).

checkSame([], _).

checkSame([person(X, Z)|L1], L2) :-
inside(person(X, L), L2),
sameList(Z, L),
sameList(L, Z),
checkSame(L1, L2).

comprLen([], []).
comprLen([H1|T1], [H2|T2]) :- comprLen(T1, T2).

checkK([person(X, Z)|G], [p(X, _)|K]) :-
checkK(G, K).

checkK([], []).

same_world(G, H, K) :-
comprLen(G, H),
checkK(G, K),
replace(G, K, R),
checkSame(R, H),
checkSame(H, R).


% optional!
% different_world(G, H)
