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

% insert_coin(Pid, Coin)

%%% IMPLEMENTATION
init() ->
  State = [], 
  close_loop(State).

close_loop(State) ->
  receive
    Msg -> Msg
  end.

% close_loop(State) ->
%   receive
%     coin ->
%       open_loop(State);
%     {enter, requester} ->
%       requester ! {error, access_denied};
%       close_loop(State);
%     stop -> terminate()
%   end.

% open_loop(State) ->
%   receive
%     {enter, requester} -> 
%       requester ! {ok},
%       close_loop(State);
%     stop -> terminate()
%   after 
    
%   end.

