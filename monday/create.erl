  
-module(create).
-export([create/1, reverse_create/1]).

create(0) -> [];
create(N) -> create(N-1) ++ [N].

reverse_create(1) -> [1];
reverse_create(N) -> [N | reverse_create(N-1)]. 
%% reverse_create(N) -> [N] ++ reverse_create(N-1).