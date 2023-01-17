%:- discontiguous check/5.

%verify(Input):-see(Input),read(T),read(L),read(S),read(F),seen,check(T,L,S,[],F),!.

%check F (dont care state)
%check(_,L,S,[],F):- member([S,A],L), member(F,A).

%check neg(F)(dont care states)
%check(_,L,S,[],neg(F)):- member([S,A],L), \+member(F,A).



%X =[[S0, [r]], [s1, [p]], [s2, [p,q,r]]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For SICStus, uncomment line below: (needed for member/2)%:- use_module(library(lists)).
% Load model, initial state and formula from file.
verify(Input) :-
    see(Input), read(T), read(L), read(S), read(F), seen,
    check(T, L, S, [], F).

%Literals
% X 
check(_, L, S, [], X) :-
    member([S, _], T),
    member([S, Labels], L), 
    member(X, Labels).

% neg(X)
check(_, L, S, [], neg(X)) :-
    member([S, _], T),
    member([S, Labels], L),
    \+member(X, Labels).

% and(F,G)
check(T, L, S, [], and(F,G)) :-
    member([S, _], T),
    member([S, Labels], L),
    member(F, Labels),
    member(G, Labels). 

% or(F,G)
check(T, L, S, [], or(F,G)) :-
    member([S, _], T),
    member([S, Labels], L),
    member(F, Labels);
    member(G, Labels). 

check_all(_, _, [], _, _).
check_all(T, L, [], U, F) :-
    check(_, L, S, X, F),
    check_all(T, L, S, Xs, F). 


% AX
check(T, L, S, U, F) :-



% EX
check(_, L, S, [], ex(F)) :-


% AG
% EG
% EF
% AF


