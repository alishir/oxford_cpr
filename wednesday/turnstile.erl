-module(turnstile).
-compile(export_all).
% -export([start/0, insert_coin/2, enter/1]).

% key choices
% init = 
% state = 
% msg = 
% fsm = closed -> open by coin, open -> closed by enter

%%% API
start() ->
  Pid = spawn(?MODULE, init, []),
  {ok, Pid}.

insert_coin(Pid, Coin) ->
  Pid ! {coin, Coin},
  ok.

enter(Pid) ->
  Pid ! {enter, self()},
  receive
    {enter_reply, Reply} -> Reply
  end.

%%% IMPLEMENTATION
init() ->
  State = [], 
  close_loop(State).

close_loop(State) ->
  io:format("Closed~n"),
  receive
    {coin, Coin} ->
      open_loop(State);
    {enter, Requester} ->
      io:format("Msg: ~p ~p~n", [enter, Requester]),
      Requester ! {enter_reply, {error, access_denied}},
      close_loop(State)
  end.

open_loop(State) ->
  io:format("Open~n"),
  receive
    {enter, Requester} -> 
      io:format("Msg: ~p ~p~n", [enter, Requester]),
      Requester ! {enter_reply, ok},
      close_loop(State)
  after 5000
    close_loop(State)
  end.

