-module(print).
-export([print/1, even_print/1]).

print(N) when N >= 0 -> 
    print_from_to_by(1, N, 1).

even_print(N) when N >= 0 ->
  print_from_to_by(2, N, 2).

print_from_to_by(From, To, By) when From + By =< To ->
  io:format("~p~n", [From]),
  print_from_to_by(From + By, To, By);
print_from_to_by(From, To, By) when From + By >= To -> 
  io:format("~p~n", [From]).  