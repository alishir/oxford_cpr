-module(sup).
-export([start/1, start_child/4, stop/1]).
-export([init/0]).

%% supervisor
% state = map of children, k = {Child, ChildMonitor}, v = {Client, AttemptsRemaining, Mod, Func, Args} 

%% API
start(SupName) ->
  register(SupName, Pid=spawn(?MODULE, init, [])),
  {ok, Pid}.

start_child(SupName, Mod, Func, Args) ->
  SupName ! {start_child, self(), Mod, Func, Args},
  receive
    {child_started, Pid} -> {ok, Pid};
    {child_error, Msg} -> Msg
  end.

stop(SupName) ->
  SupName ! {stop, self()},
  receive 
    {children_killed, ok} -> ok
  end.

%% SETUP
init() ->
  supervisor_loop(maps:new()).

%% LOOP
supervisor_loop(ChildrenProcesses) ->
  receive
    {start_child, Client, Mod, Func, Args} ->
      spawn_monitor_child(ChildrenProcesses, Client, 5, Mod, Func, Args);
    {'DOWN', ChildMonitor, process, Child, Reason} ->
      {Client, AttemptsRemaining, Mod, Func, Args} = maps:get({Child, ChildMonitor}, ChildrenProcesses),
      {UpdatedChildrenProcesses, UpdatedAttemptsRemaining} = demonitor_child(ChildrenProcesses, Child, ChildMonitor, AttemptsRemaining),
      notify_client(Client, Child, UpdatedAttemptsRemaining, Mod, Func, Reason),
      spawn_monitor_child(UpdatedChildrenProcesses, Client, UpdatedAttemptsRemaining, Mod, Func, Args);
    {stop, Client} ->
      maps:fold(fun({Child, _}, _, _) -> exit(Child, kill) end, ok, ChildrenProcesses),
      Client ! {children_killed, ok},
      ok
  end.

%% HELPER FUNCTIONS  
spawn_monitor_child(ChildrenProcesses, _, 0, _, _, _) ->
  supervisor_loop(ChildrenProcesses);
spawn_monitor_child(ChildrenProcesses, Client, AttemptsRemaining, Mod, Func, Args) ->
  Child = spawn(Mod, Func, Args),
  ChildMonitor = monitor(process, Child),
  Client ! {child_started, Child},
  supervisor_loop(
    maps:put({Child, ChildMonitor}, 
             {Client, AttemptsRemaining, Mod, Func, Args}, ChildrenProcesses)
  ).

notify_client(Client, Child, 0, _, _, Reason) ->
  Client ! {child_error, 
    io:format("-----------------------------------------------
Error: Process ~p Terminated ~p time(s)
Reason for termination: ~p.
Will not restart
-----------------------------------------------~n", 
    [Child, 5, Reason])};
notify_client(Client, Child, AttemptsRemaining, Mod, Func, Reason) ->
  Client ! {child_error, 
    io:format("-----------------------------------------------
Error: Process ~p Terminated ~p time(s)
Reason for termination: ~p
Restarting with ~p:~p
-----------------------------------------------~n", 
    [Child, 5 - AttemptsRemaining, Reason, Mod, Func])}.


demonitor_child(ChildrenProcesses, Child, ChildMonitor, AttemptsRemaining) ->
  demonitor(ChildMonitor, [flush]),
  {maps:remove({Child, ChildMonitor}, ChildrenProcesses), AttemptsRemaining - 1}.


  