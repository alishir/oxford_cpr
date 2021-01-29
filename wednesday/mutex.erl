-module(mutex).

-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

%% Original implementation problems
% 1. If client 1 crashes in between sending wait and signal then problem
% 2. If client 2 is waiting and crashes then problem

%% Solution
% 1. As both use call can simply monitor the client in call and only demonitor
% once you receive a reply.
% 2. Have process_flag on the server side and link
% 3. Have process_flag on the client side as well - might be dangerous!

%% CLIENT API ===============================================================
start() ->
  process_flag(trap_exit, true),
  register(mutex, spawn(?MODULE, init, [])).

stop() ->
  mutex ! stop.
  
wait() ->
  call(wait).

signal() ->
  call(signal).

%% MUTEX FSM STATES =========================================================
free() ->
  receive 
    {wait, Pid} ->
      link(Pid),
      reply(Pid, ok),
      busy(Pid)
  end.

busy(Pid) ->
  receive 
    {signal, Pid} ->
      reply(Pid, ok),
      unlink(Pid),
      free()
    {'EXIT', Pid, Reason} ->
      free()
  end.

%% HELPER FUNCTIONS =========================================================
init() ->
  process_flag(trap_exit, true),
  free().

call(Message) ->
  mutex -> {Message, self()},
  receive
    {reply, Reply} -> 
      Reply;
    {'EXIT', _Pid, Reason -> 
      {error, Reason} 
  end.

reply(Pid, Message) ->
  Pid ! {reply, Message}.

