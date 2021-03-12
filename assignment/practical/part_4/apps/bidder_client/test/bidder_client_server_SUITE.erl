%%%----------------------------------------------------------------------------
%% File: bidder_client_server_SUITE.erl
%% @author Nicholas Drake
%% @doc bidder_client_server_SUITE
%% @end
%%%----------------------------------------------------------------------------
-module(bidder_client_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2, 
         end_per_testcase/2]).
-export([test_start_link/1,
         test_stop/1,
         test_get_auctions/1]).

all() -> 
  [test_start_link,
   test_stop,
   test_get_auctions].

%%% suite setup & tear down ---------------------------------------------------
init_per_suite(Config) ->
  ok = application:start(auction_server),
  Config.

end_per_suite(_Config) ->
  application:stop(auction_server),
  ok.

%%% group setup & tear down ---------------------------------------------------
init_per_group(_, Config) ->
  Config.

end_per_group(_, _Config) ->
  ok.

%%% testcase setup ------------------------------------------------------------
init_per_testcase(test_start_link, Config) ->
  BidderName1 = "elon musk",
  BidderName2 = "jeff bezos",
  [{bidder_names, [BidderName1, BidderName2]} | Config];
init_per_testcase(_, Config) ->
  BidderName1 = "elon musk",
  BidderName2 = "jeff bezos",
  {ok, ClientPid1} = bidder_client_server:start_link(BidderName1),
  {ok, ClientPid2} = bidder_client_server:start_link(BidderName2),
  [{bidder_names, [BidderName1, BidderName2]} | 
    [{client_pids, [ClientPid1, ClientPid2]} | 
      Config]].

%%% testcase teardown ---------------------------------------------------------
end_per_testcase(test_start_link, Config) ->
  ok;
end_per_testcase(test_stop, Config) ->
  [_, BidderName2] = ?config(bidder_names, Config),
  ok = bidder_client_server:stop(BidderName2);
end_per_testcase(_, Config) ->
  [BidderName1, BidderName2] = ?config(bidder_names, Config),
  ok = bidder_client_server:stop(BidderName1),
  ok = bidder_client_server:stop(BidderName2).

%%% unit tests ----------------------------------------------------------------
test_start_link(Config) ->
  [BidderName1, BidderName2] = ?config(bidder_names, Config),
  {ok, ClientPid1} = bidder_client_server:start_link(BidderName1),
  {ok, ClientPid2} = bidder_client_server:start_link(BidderName2),
  true = (ClientPid1 =/= ClientPid2).

test_stop(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  [ClientPid1, ClientPid2] = ?config(client_pids, Config),
  ok = bidder_client_server:stop(BidderName1),
  false = is_process_alive(ClientPid1),
  true = is_process_alive(ClientPid2).

test_get_auctions(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  ok = bidder_client_server:get_auctions(BidderName1),
  {ok, AuctionId1} = auction_data:create_auction(),
  % [AuctionId1] = bidder_client_server:get_auctions(BidderName1),
  % {ok, AuctionId2} = auction_data:create_auction(),
  ok.
