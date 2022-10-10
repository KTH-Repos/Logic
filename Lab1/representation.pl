member(X,L) :- select(X,L,_).

reverse_accumulator([],A,A).
reverse_accumulator([H|T],A,R) :- reverse_accumulator(T,[H|A],R).

reverse(L,R) :- reverse_accumulator(L,[],R).


remove_duplicates(L,N) :- duplicate_accumulator(L,[],M), reverse(M,N).

duplicate_accumulator([], A, A).
duplicate_accumulator([H|T], A, L) :- 
    member(H,A),
    duplicate_accumulator(T,A,L).

duplicate_accumulator([H|T],A,L) :- 
    duplicate_accumulator(T,[H|A],L).