verify(InputFileName) :- 
    see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    %checkGoal(Goal, Proof),
    valid_proof(Prems, Goal, Proof, Verifiedlines).

valid_proof(_, _, [], _).

valid_proof(Prems, Goal, [X,Y|T], Verifiedlines) :-
    premise(Prems, X); findbox(Prems, Goal, X, Verifiedlines);
    andint(X, Verifiedlines); andel1(X, Verifiedlines); andel2(X, Verifiedlines); 
    orint1(X, Verifiedlines); orint2(X, Verifiedlines); impel(X, Verifiedlines);
    negel(X, Verifiedlines); mt(X, Verifiedlines); lem(X, Verifiedlines); 
    negnegint(X, Verifiedlines); negnegel(X, Verifiedlines); contel(X, VerifiedLines); 
    copy(X, Verifiedlines); orel(X, Verifiedlines);
    impint(X, Verifiedlines); negint(X, Verifiedlines); pbc(X, Ver), valid_proof(Prems, Goal, [Y|T], [H|Verifiedlines]).

valid_proof(Prems, Goal, [X|[]], Bevis) :-
premise(Prems, X); andint(X,Bevis); andel1(X, Bevis); andel2(X, Bevis); orint1(X, Bevis); orint2(X, Bevis);
impel(X, Bevis); negnegint(X,Bevis); copy(X,Bevis); negnegel(X,Bevis); negel(X,Bevis); impint(X,Bevis); orel(X,Bevis);
contel(X,Bevis); negint(X,Bevis); lem(X,Bevis); mt(X, Bevis); pbc(X, Bevis), goal(X, Goal).


goal(X,Goal) :-
  member(Goal, X).


/* checkGoal(Goal, Proof) :- 
    last(Proof, LastRow),
    nth1(2,LastRow,Goal).
 */
%%check premise
premise(Prems, [_, P, premise]) :- member(P, Prems). 

%%andint
andint([Row, and(P,Q), andint(R1, R2)], Verifiedlines):-
    Row > R1, Row > R2,
    member([R1, P, _], Verifiedlines), 
    member([R2, Q, _], Verifiedlines).

%%andel1
andel1([_, P, andel1(Row)], Verifiedlines) :-
    member([Row, and(P,_), _], Verifiedlines).

%%andel2
andel2([_, Q, andel2(Row)], Verifiedlines) :-
    member([Row, and(_,Q), _], Verifiedlines).

%%orint1
orint1([_, or(P,Q), orint1(Row)], Verifiedlines) :- 
    member([Row, P, _], Verifiedlines). 

%%orint2
orint2([_,or(P,Q), orint2(Row)], Verifiedlines) :-
    member([Row, Q, _], Verifiedlines). 

%%impel
impel([Row,Q,impel(R1,R2)],Verifiedlines) :-
    member([R1, P, _], Verifiedlines), member([R2, imp(P,Q), _], Verifiedlines), Row > R1, Row > R2.

%%negel
negel([Row, cont, negel(R1, R2)], Verifiedlines) :-
    member([R1, P, _], Verifiedlines), member([R2, neg(P), _], Verifiedlines). 

%%MT
mt([Row,neg(P),mt(R1,R2)], Verifiedlines) :- 
    member([R1,imp(P,Q),_], Verifiedlines), member([R2,neg(Q),_], Verifiedlines), Row > R1, Row > R2.

%%LEMDISC
lem([_, or(X,Y), lem], Verifiedlines) :-
    member(X,[neg(Y)]); member(Y,[neg(X)]).

%%negnegint
negnegint([_, negneg(P), negnegint(Row)], Verifiedlines) :-
    member([Row, P, _], Verifiedlines). 

%%negnegel
negnegel([_, P, negnegel(Row)], Verifiedlines) :-
    member([Row, negneg(P), _], Verifiedlines). 

%%contel
contel([_, _, contel(Row)], Verifiedlines) :-
    member([Row, cont, _], Verifiedlines). 

%%copy
copy([_, P, copy(Row)], Verifiedlines):- 
    member([Row, P, _], Verifiedlines). 

%%findbox
findbox(Prems, Goal, [[_, _, assumption]|T], Verifiedlines) :- 
    valid_proof(Prems, Goal, T, [[_, _, assumption]|Verifiedlines]).

%%orel
orel([_, Ans, orel(X, Y, U, V, W)], Verifiedlines) :- %ha med Row och jämför? 
    member(List1, Verifiedlines),
    member(List2, Verifiedlines),
    member([X, or(P,Q), _], Verifiedlines),
    member([Y, P, assumption], List1),
    member([U, Ans, _], List1),
    member([V, Q, assumption], List2),
    member([W, Ans, _], List2).

%%impint
impint([_, imp(P,Q), impint(R1,R2)], Verifiedlines):-
    member(List, Verifiedlines), 
    member([R1, P, assumption], List),
    member([R2, Q, _], List).
    
%%negint
negint([_, neg(P), negint(R1, R2)], Verifiedlines) :-
    member(List, Verifiedlines),
    member([R1, P, _], List),
    member([R2, cont, _], List).

%%PBC
pbc([_, P, pbc(R1, R2)], Verifiedlines) :-
    member(List, Verifiedlines),
    member([R1, neg(P), _], List),
    member([R2, cont, _], List).