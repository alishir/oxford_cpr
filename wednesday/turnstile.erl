-module(turnstile).
-compile(export_all).
% -export([start/0, insert_coin/2, enter/1]).

% key choices
% init = 
% State = MoneyInserted & FeeRequired 
% msg = atom_id & Requester or Coin
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
  MoneyInserted = 0,
  FeeRequired = 5, 
  close_loop(MoneyInserted, FeeRequired).

close_loop(MoneyInserted, FeeRequired) ->
  io:format("Closed~n"),
  receive
    {coin, Coin} ->
      if 
        MoneyInserted + Coin >= FeeRequired ->
          open_loop(0, FeeRequired); % no change
        true ->
          close_loop(MoneyInserted + Coin, FeeRequired)
      end;
    {enter, Requester} ->
      % io:format("Msg: ~p ~p~n", [enter, Requester]),
      Requester ! {enter_reply, {error, access_denied}},
      close_loop(MoneyInserted, FeeRequired)
  end.

open_loop(_, FeeRequired)->
  io:format("Open~n"),
  receive
    {enter, Requester} -> 
      % io:format("Msg: ~p ~p~n", [enter, Requester]),
      Requester ! {enter_reply, ok},
      close_loop(0, FeeRequired)
  after 5000 ->
    close_loop(0, FeeRequired)
  end.
