-module(sum).
-export([sum/1, sum_interval/2]).

sum(0) -> 0;
sum(N) -> N + sum(N-1).

sum_interval(N, N) -> N;
sum_interval(N, M) when N < M ->
    M + sum_interval(N, M -1).
%% don't handle N > M so fails abnormally    