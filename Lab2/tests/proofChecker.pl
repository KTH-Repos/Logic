
verify(InputFileName) :- 
    see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    valid_proof(Prems, Goal, Proof, []),!.


valid_proof(_, _, [], _).

valid_proof(Prems, Goal, [H|T], Verifiedlines) :-
    check_row(Prems, Goal, H, Verifiedlines), 
    valid_proof(Prems, Goal, T, [H|Verifiedlines]).


check_row(Prems, Goal, [H|[]], ValidProof) :-
    check_row(Prems, Goal, H, ValidProof),
    member(Goal, H).

%%check premise
%premise(Prems, [_, P, premise]) :- member(P, Prems). 
check_row(Prems, _, [_, P, premise], VerifiedLines) :- member(P,Prems).

%%andint
check_row(_,_,[Row, and(P,Q), andint(R1, R2)], Verifiedlines):-
    Row > R1, Row > R2, 
    member([R1, P, _], Verifiedlines), 
    member([R2, Q, _], Verifiedlines).

%%andel1
check_row(_,_,[_, P, andel1(Row)], Verifiedlines) :-
    member([Row, and(P,_), _], Verifiedlines).

%%andel2
check_row(_,_,[_, Q, andel2(Row)], Verifiedlines) :-
    member([Row, and(_,Q), _], Verifiedlines).

%%orint1
check_row(_,_,[_, or(P,Q), orint1(Row)], Verifiedlines) :- 
    member([Row, P, _], Verifiedlines). 

%%orint2
check_row(_,_,[_,or(P,Q), orint2(Row)], Verifiedlines) :-
    member([Row, Q, _], Verifiedlines). 

%%impel
check_row(_,_,[Row,Q,impel(R1,R2)],Verifiedlines) :-
    member([R1, P, _], Verifiedlines), member([R2, imp(P,Q), _], Verifiedlines), Row > R1, Row > R2.

%%negel
check_row(_,_,[Row, cont, negel(R1, R2)], Verifiedlines) :-
    member([R1, P, _], Verifiedlines), member([R2, neg(P), _], Verifiedlines). 

%%MT
check_row(_,_,[Row,neg(P),mt(R1,R2)], Verifiedlines) :- 
    member([R1,imp(P,Q),_], Verifiedlines), member([R2,neg(Q),_], Verifiedlines), Row > R1, Row > R2.

%%LEMDISC
check_row(_,_,[_, or(P, neg(P)), lem], Verifiedlines).
    %member([_, _, _ ], Verifiedlines). 

%%negnegint
check_row(_,_,[_, negneg(P), negnegint(Row)], Verifiedlines) :-
    member([Row, P, _], Verifiedlines). 

%%negnegel
check_row(_,_,[_, P, negnegel(Row)], Verifiedlines) :-
    member([Row, negneg(P), _], Verifiedlines). 

%%contel
check_row(_,_,[_, _, contel(Row)], Verifiedlines) :-
    member([Row, cont, _], Verifiedlines). 

%%copy
check_row(_,_,[_, P, copy(Row)], Verifiedlines):- 
    member([Row, P, _], Verifiedlines). 

%%findbox
check_row(Prems, Goal, [[_, _,assumption]|T], Verifiedlines) :-
    valid_proof(Prems, Goal, T, [[_, _, assumption]|Verifiedlines]).

%%orel
check_row(_,_,[_, Ans, orel(X, Y, U, V, W)], Verifiedlines) :-
    member(List1, Verifiedlines),
    member(List2, Verifiedlines),
    member([X, or(P,Q), _], Verifiedlines),
    member([Y, P, assumption], List1),
    member([U, Ans, _], List1),
    member([V, Q, assumption], List2),
    member([W, Ans, _], List2).

%%impint
check_row(_,_,[_, imp(P,Q), impint(R1,R2)], Verifiedlines):-
    member(List, Verifiedlines), 
    member([R1, P, assumption], List),
    member([R2, Q, _], List).
    
%%negint
check_row(_,_,[_, neg(P), negint(R1, R2)], Verifiedlines) :-
    member(List, Verifiedlines),
    member([R1, P, _], List),
    member([R2, cont, _], List).

%%PBC
check_row(_,_,[_, P, pbc(R1, R2)], Verifiedlines) :-
    member(List, Verifiedlines),
    member([R1, neg(P), assumption], List),
    member([R2, cont, _], List).
    