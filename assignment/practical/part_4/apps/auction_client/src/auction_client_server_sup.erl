%%%----------------------------------------------------------------------------
%% File: auction_client_server_sup.erl
%% @author Nicholas Drake
%% @doc Bidder client supervisor.
%% @end
%%%----------------------------------------------------------------------------
-module(auction_client_server_sup).

-behaviour(supervisor).

-export([start_link/1, stop/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(BidderName) ->
  BidderNameSup = BidderName ++ " supervisor",
  supervisor:start_link({global, BidderNameSup}, ?MODULE, [BidderName]).

stop(BidderName) ->  
  process_flag(trap_exit, true),
  exit(global:whereis_name(BidderName), shutdown).

init([BidderName]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [{auction_client_child, 
                  {auction_client_server, start_link, [BidderName]},
                  transient, 1000, worker, [auction_client_server]}],
    {ok, {SupFlags, ChildSpecs}}.
