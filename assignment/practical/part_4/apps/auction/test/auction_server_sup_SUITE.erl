%%%----------------------------------------------------------------------------
%% File: auction_sup_SUITE.erl
%% @author Nicholas Drake
%% @doc Auction supervisor test suite
%% @end
%%%----------------------------------------------------------------------------
-module(auction_server_sup_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([test_start_link/1,
        test_stop/1]).

all() -> [].
  % [test_start_link,
  %  test_stop].
% you can run these tests by uncommenting - however when run all together 
% the time for mnesia application to close can cause knock on effects to this 
% test so they fail in loading mnesia as mnesia is still closing for another 
% test about 10% of the time.

%%% testcase setup ------------------------------------------------------------
init_per_testcase(test_start_link, Config) ->
  Config;
init_per_testcase(test_stop, Config) ->
  {ok, _SupervisorPid} = auction_server_sup:start_link(),
  Config.

end_per_testcase(test_start_link, _Config) ->
  ok = auction_server_sup:stop();
end_per_testcase(test_stop, _Config) ->
  ok.

%% supervisor test -----------------------------------------------------------
test_start_link(_Config) ->
  {ok, SupervisorPid} = auction_server_sup:start_link(),
  {error, {already_started, SupervisorPid}} = auction_server_sup:start_link(),
  {ok, AuctionId} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId),
  {ok, _MonitorRef} = auction:subscribe(AuctionId).

test_stop(_Config) ->
  true = auction_server_sup:stop().