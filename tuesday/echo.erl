-module(echo).
-export([start/0, stop/0, print/1, loop/0]).

loop() ->
  receive
    stop -> ok;
    Msg ->
      io:format("~p~n", [Msg]),
      loop()
  end.

start() ->
  register(echo, spawn(echo, loop, [])),
  ok.

print(Term) ->
  echo ! Term,
  ok.

stop() ->
  echo ! stop,
  ok.
