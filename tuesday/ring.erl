-module(ring).
-export([start/1, message/2, quit/0]).
-export([init/2, init/3]).
% -compile(export_all).

% 2 strategies
% 1. central process that sets up the ring and initiates message sending
% 2. each process spawns the next process in the ring, but need a way to connect the last to the first

% Further questions
% 1. should setup args or setup message handle I, N & FirstPid? setups args is fine as not preserved as state
% 2. should ring or message handle whether go to start of ring again? message should be abstracted away from ring size so ring,
%    therefore need each process to know its number in ring , i.e. it needs to know I, actually can avoid having 
%    it needing to know I by just having it have a start node, and sending that start node address and decrementing when the 
%    it gets back around to that start node again
% 3. should state change? probably not because we want the message to count number of times so handle() function 
%     will not update the state

% each process should
% 1. state = next
% 2. setup args = I, N, FirstPid
% 3. message sent = Msg, M, OneLoopPid, 
start(N) ->
  register(driver, spawn(ring, init, [1, N])),
  ok. % can't stop this printing out to console
message(Msg, M) ->
  driver ! {Msg, M, whereis(driver)},
  ok. % can't stop this printing out to console

quit() ->
  driver ! quit.

init(I, N) ->
  io:format("Ring: ~n~p=~p -> ~n", [I, self()]),
  Next = initialize_args(I, N, self()),
  loop(Next).
init(I, N, FirstPid) ->
  io:format("~p=~p -> ~n", [I, self()]),
  Next = initialize_args(I, N, FirstPid),
  loop(Next).

initialize_args(N, N, FirstPid) -> 
  FirstPid;
initialize_args(I, N, FirstPid) ->
  spawn(ring, init, [I+1, N, FirstPid]).

loop(Next) ->
  receive 
    {Msg, M, OneLoopPid} -> 
      io:format("~p -> ~p [Msg=~p (~p x)]~n", 
        [self(), Next, Msg, M]),
      handle(OneLoopPid, Msg, M, Next),
      loop(Next);
    quit -> 
      Next ! quit
  end.

% completed M loops
handle(Next, _, 1, Next) -> ok;
% completed 1 loop i.e. Next = OneLoopPid
handle(Next, Msg, M, Next) ->
  Next ! {Msg, M-1, Next};
% in loop
handle(OneLoopPid, Msg, M, Next) ->
  Next ! {Msg, M, OneLoopPid}.