%%partstring/3 : 
%% 1:a argument --> en given lista
%% 2:a argument --> längden av listan F
%% 3:e argument --> en lista F med längden L som man finner konsekutivt i den första listan

lengthList([],0).
lengthList([_|T],N) :- lengthList(T,N1), N is N1+1.

prefix(_,[]).
prefix([X|Xs],[X|Ys]) :-
    prefix(Xs,Ys).

subset([], []).
subset([H|T], [H|R]) :- 
    prefix(T,R),
    subset(T, R).
subset([_|T], R) :- 
    subset(T, R).


partstring([],_,[]).
partstring(X,N,Y) :-
    subset(X,Y),
    lengthList(Y,N),
    Y\=[].
