%%%----------------------------------------------------------------------------
%% File: auction_sup.erl
%% @author Nicholas Drake
%% @doc auction top level supervisor.
%% @end
%%%----------------------------------------------------------------------------

-module(auction_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_auction/1,
         stop_auction/1]).

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
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5, % max number of restarts / period
                 period => 60}, % period is 60
    ChildSpecs = [{auction_child,
                   {auction, start_link, []},
                   transient, 1000, worker, [auction]}],
    {ok, {SupFlags, ChildSpecs}}.

start_auction(AuctionId) ->
  supervisor:start_child(?MODULE, [AuctionId]).

stop_auction(AuctionPid) ->
  supervisor:terminate_child(?MODULE, AuctionPid).

%% internal functions