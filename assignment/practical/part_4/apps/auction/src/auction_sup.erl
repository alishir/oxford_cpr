%%%----------------------------------------------------------------------------
%% File: auction_sup.erl
%% @author Nicholas Drake
%% @doc auction top level supervisor.
%% @end
%%%----------------------------------------------------------------------------

-module(auction_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10, % max number of restarts / hour
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

start_auction(AuctionId) ->
  ChildSpec = {AuctionId, 
               {auction, start_link, [AuctionId]}, 
               transient,
               2000,
               worker,
               auction},
  supervisor:start_child(?MODULE, ChildSpec).

%% internal functions