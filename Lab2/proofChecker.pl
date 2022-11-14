
% första steg lär vara att kolla om första raden i beviset är premiss. 

verify(InputFileName) :- see(InputFileName),
read(Prems), read(Goal), read(Proof),
seen,
valid_proof(Prems, Goal, Proof).

