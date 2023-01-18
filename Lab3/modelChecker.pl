% For SICStus, uncomment line below: (needed for member/2)%:- use_module(library(lists)).
% Load model, initial state and formula from file.
verify(Input) :-
    see(Input), read(T), read(L), read(S), read(F), seen,
    check(T, L, S, [], F).

check_all(_, _, [], _, _).
check_all(T, L, [S|Tail], U, F) :-
    check(T, L, S, U, F),
    check_all(T, L, Tail, U, F). 

check_some(T, L, [Q1|Tail], U, X) :-
    check(T, L, Q1, U, X);
    check_some(T, L, Tail, U, X).

%Literals
% X 
check(_, L, S, [], X) :-
    member([S, Labels], L), 
    member(X, Labels).

% neg(X)
check(_, L, S, [], neg(X)) :-
    member([S, Labels], L),
    \+member(X, Labels).

% and(F,G)
check(T, L, S, [], and(F,G)) :-
    check(T, L, S, [], F),
    check(T, L, S, [], G).

% or1(F,G)
check(T, L, S, [], or(F,G)) :-
    check(T, L, S, [], F);
    check(T, L, S, [], G).

% AX
check(T, L, S, [], ax(F)) :-
    member([S, Neighbors], T),
    check_all(T, L, Neighbors, [], F). 

% EX
check(T, L, S, [], ex(F)) :-
    member([S, Neighbors], T), 
    check_some(T, L, Neighbors, [], F). 

% AG1
check(_, _, S, U, ag(_)) :-
    member(S, U).

% AG2
check(T, L, S, U, ag(F)) :-
    \+member(S, U),
    check(T, L, S, [], F),
    member([S, Neighbors], T),
    check_all(T, L, Neighbors, [S|U], ag(F)).

% EG1
check(_, _, S, U, eg(_)):-
    member(S, U).

% EG2
check(T, L, S, U, eg(F)):-
    \+ member(S, U),
    check(T, L, S, [], F),
    member([S,Neighbors], T),
    check_some(T, L, Neighbors, [S|U], eg(F)).

% EF1
check(T, L, S, U, ef(F)) :-
    \+member(S, U),
    check(T, L, S, [], F).

% EF2
check(T, L, S, U, ef(F)) :-
    \+member(S, U),
    member([S,Neighbors], T),
    check_some(T, L, Neighbors, [S|U], ef(F)).

% AF1
check(T, L, S, U, af(F)) :-
    \+member(S, U),
    check(T, L, S, [], F).

% AF2
check(T, L, S, U, af(F)) :-
    \+member(S, U),
    member([S,Neighbors], T),
    check_all(T, L, Neighbors, [S|U], af(F)). 