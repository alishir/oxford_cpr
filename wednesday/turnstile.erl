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
  {ok, spawn(?MODULE, init, [])}.

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
  close(MoneyInserted=0, FeeRequired=5).

close(MoneyInserted, FeeRequired) ->
  io:format("Closed~n"),
  receive
    {coin, Coin} ->
      NewMoneyInserted = MoneyInserted + Coin
      if 
        NewMoneyInserted >= FeeRequired ->
          open(0, FeeRequired); % no change
        true ->
          close(NewMoneyInserted, FeeRequired)
      end;
    {enter, Requester} ->
      % io:format("Msg: ~p ~p~n", [enter, Requester]),
      Requester ! {enter_reply, {error, access_denied}},
      close(MoneyInserted, FeeRequired)
  end.

open(_, FeeRequired)->
  io:format("Open~n"),
  receive
    {enter, Requester} -> 
      % io:format("Msg: ~p ~p~n", [enter, Requester]),
      Requester ! {enter_reply, ok},
      close(0, FeeRequired)
  after 5000 ->
    close(0, FeeRequired)
  end.
