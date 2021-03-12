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

-export([test_stop/1]).

all() ->
  [test_stop].

%%% testcase setup ------------------------------------------------------------
init_per_testcase(test_stop, Config) ->
  {ok, _SupervisorPid} = auction_server_sup:start_link(), % test start_link
  Config.

end_per_testcase(test_stop, _Config) ->
  ok.

%% supervisor test -----------------------------------------------------------
test_stop(_Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId),
  {ok, _MonitorRef} = auction:subscribe(AuctionId),
  ok = auction_data:remove_auction(AuctionId),
  true = auction_server_sup:stop().