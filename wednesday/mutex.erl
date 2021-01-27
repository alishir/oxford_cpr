-module(mutex).

-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

%% API ======================================================================
start() ->
  register(mutex, spawn(?MODULE, init, [])).

stop() ->
  mutex ! stop.
  
wait() ->
  call(wait).

signal() ->
  call(signal).

%% FSM STATES ===============================================================
free() ->
  receive 
    {wait, Pid} ->
      reply(Pid, ok),
      busy(Pid)
  end.

busy(Pid) ->
  receive 
    {signal, Pid} ->
      reply(Pid, ok),
      free()
  end.

%% HELPER FUNCTIONS =========================================================
init() ->
  free().

call(Message) ->
  mutex -> {Message, self()},
  receive
    {reply, Reply} -> reply
  end.

reply(Pid, Message) ->
  Pid ! {reply, Message}.

