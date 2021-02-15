% Rudimentary test suite. Feel free to replace anything

% Can run as: swipl -g run_tests,halt src/instahub.pl tests/instatest.pl

% The sample graphs from the assignment text:
g1([person(kara, [barry, clark]),
    person(bruce,[clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).

g2([person(batman, [green_arrow, superman]),
    person(green_arrow, [supergirl]),
    person(supergirl, [flash, superman]),
    person(flash, [green_arrow, supergirl]),
    person(superman, [green_arrow, supergirl])]).

:- begin_tests(instahub).

test(follows1, [nondet]) :-
    g1(G), follows(G, bruce, clark).

test(follows2, [fail]) :-
    g1(G), follows(G, clark, bruce).

test(follows3, [set(X == [barry,clark,oliver])]) :-
    g1(G), follows(G, X, kara).

test(follows4, [set(X == [oliver,kara])]) :-
    g1(G), follows(G, clark, X).

test(ignores1, [nondet]) :-
    g1(G), ignores(G, clark, bruce).

test(ignores2, [fail]) :-
    g1(G), ignores(G, clark, barry).

test(ignores3, [set(X == [clark,oliver])]) :-
    g1(G), ignores(G, X, bruce).

test(popular1, [nondet]) :- 
    g1(G), popular(G, kara).

test(popular2, [fail]) :- 
    g1(G), popular(G, clark).

test(popular3, [set(X == [kara])]) :- 
    g1(G), popular(G, X).

test(outcast1, [nondet]) :-
    g1(G), outcast(G, bruce).

test(outcast2, [fail]) :-
    g1(G), outcast(G, clark).

test(outcast3, [set(X == [bruce,oliver])]) :-
    g1(G), outcast(G, X).

test(friendly1, [nondet]) :-
    g1(G), friendly(G, barry).

test(friendly2, [fail]) :-
    g1(G), friendly(G, kara).

test(friendly3, [set(X == [barry,bruce])]) :-
    g1(G), friendly(G, X).

test(hostile1, [nondet]) :-
    g1(G), hostile(G, oliver).

test(hostile2, [fail]) :-
    g1(G), hostile(G, barry).

test(hostile3, [set(X == [bruce,oliver])]) :-
    g1(G), hostile(G, X).

test(aware1, [nondet]) :-
    g1(G), aware(G, bruce, kara).

test(aware2, [fail]) :-
    g1(G), aware(G, kara, bruce).

test(aware3, [set(X == [kara,bruce,barry])]) :-
    g1(G), aware(G, X, clark).

test(aware4, [set(X == [barry,clark,oliver])]) :-
    g1(G), aware(G, kara, X).

test(aware5, [fail]) :-
    g1(G), aware(G, kara, kara).

test(ignorant1, [nondet]) :-
    g1(G), ignorant(G, kara, bruce).

test(ignorant2, [fail]) :-
    g1(G), ignorant(G, bruce, bruce).

test(ignorant3, [fail]) :-
    g1(G), ignorant(G, bruce, kara).

test(ignorant4, [set(X == [kara,barry,clark,oliver])]) :-
    g1(G), ignorant(G, X, bruce).

test(same_world1, [set(X == [])]) :-
    same_world([], [], X).

test(same_world2, [fail]) :-
    g1(G), same_world(G, [], _).

test(same_world3, [set(X == [p(kara, supergirl), p(bruce, batman), p(barry, flash), p(clark, superman), p(oliver, green_arrow)])]) :-
    g1(G1), g2(G2), same_world(G1, G2, X).

:- end_tests(instahub).
