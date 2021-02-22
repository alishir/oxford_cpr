-module(frequency_otp).
-behaviour(gen_server).

-export([start_link/0, init/1, handle/2, allocate/0, deallocate/1]).

%% the client functions
start_link() ->
  gen_server:start_link({local, frequency}, frequency, [], []).
  
allocate() ->
  gen_server:call(frequency, {allocate, self()}).

deallocate(Freq) ->
  gen_server:cast(frequency, {deallocate, Freq}).

stop() ->
  gen_server:cast(frequency, stop).

% setup
init(_Args) ->
  Free = get_frequencies(), % synchronous because we want to detect failure immediately.
  Allocated = []
  {ok, {Free, Allocated}}.

get_frequencies() -> [10, 11, 12, 13, 14, 15].

%% server functions
handle_call({allocate, Pid}), _From, Frequencies) ->
  {NewFrequencies, Reply} = allocate(Frequencies, Pid),
  {reply, Reply, NewFrequencies}.

handle_cast({deallocate, Freq}, Frequencies) ->
  NewFrequencies = deallocate(Frequencies, Freq),
  {noreply, NewFrequencies}.

handle_cast(stop, Frequencies) ->
  {stop, normal, Frequencies}.

terminate(Reason, {Free, Allocated}) ->
  % Clean up on termination
  ok.