%%%----------------------------------------------------------------------------
%% File: pubsub_sup.erl
%% @author Nicholas Drake
%% @doc Pubsub supervisor.
%% @end
%%%----------------------------------------------------------------------------

-module(pubsub_sup).

-behaviour(supervisor).

-export([start_link/0, stop/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%% API -----------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() ->
  process_flag(trap_exit, true),
  exit(whereis(?MODULE), shutdown).

%%% Internal functions --------------------------------------------------------
init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 3, % max number of restarts / period
               period => 3600, % period is 60 mins
               shutdown => 1000}, 
  ChildSpecs = [{pubsub,
                 {pubsub, start_link, []},
                 permanent, 1000, worker, [pubsub]}],
  {ok, {SupFlags, ChildSpecs}}.