%   1----------4----------5
%    \        / \         |
%     \      /   \        | 
%      \    /     \       |
%       \  /       \      |
%        2----------3-----6

edge(1,2).
edge(1,4).
edge(2,4).
edge(2,3).
edge(3,6).
edge(3,4).
edge(4,5).
edge(5,6).

select(X,[X|T],T).
select(X,[Y|T],[Y|R]) :- select(X,T,R).

member(X,L) :- select(X,L,_).

reverse_accumulator([],A,A).
reverse_accumulator([H|T],A,R) :- reverse_accumulator(T,[H|A],R).

own_reverse(L,R) :- reverse_accumulator(L,[],R).

joined(X,Y) :- edge(X,Y).
joined(X,Y) :- edge(Y,X).

traverse(U,V,_,[U|V]) :- joined(U,V).

traverse(U,V,Visited,Path) :-
    joined(U,W),
    W\=V,
    \+member(W,Visited),
    traverse(W,V,[U|Visited],Path).

path(U,V,Path) :-
    traverse(U,V,[],W),
    own_reverse(W,Path).

    