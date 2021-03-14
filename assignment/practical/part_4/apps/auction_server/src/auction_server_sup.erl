%%%----------------------------------------------------------------------------
%% File: auction_server_sup.erl
%% @author Nicholas Drake
%% @doc Auction Server supervisor.
%% @end
%%%----------------------------------------------------------------------------

-module(auction_server_sup).

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
  application:load(mnesia),
  application:load(auction_data),
  auction_data:install([node()]),
  application:start(mnesia),
  application:start(auction_data),
  SupFlags = #{strategy => rest_for_one,
               intensity => 3, % max number of restarts / period
               period => 3600}, % period is 60 mins
  ChildSpecList = [child(pubsub_sup), child(auction_sup)],
  {ok, {SupFlags, ChildSpecList}}.

child(Module) ->
  {Module, {Module, start_link, []},
   permanent, 2000, worker, [Module]}.
