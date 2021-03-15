%%%----------------------------------------------------------------------------
%% File: race_conditions_SUITE.erl
%% @author Nicholas Drake
%% @doc race_conditions_SUITE
%% @end
%%%----------------------------------------------------------------------------
-module(race_conditions_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2, 
         end_per_testcase/2]).

-export([auction_house/1,
         race_bidder_1/1,
         race_bidder_2/1]).

all() -> 
  [].
  % [{group, race_integ}].

groups() ->
  [{race_integ, 
    [], 
    [{group, race_integ_components}]},
   {race_integ_components, 
    [parallel, {repeat, 10}], 
    [auction_house, race_bidder_1, race_bidder_2]}].

%%% suite setup & tear down ---------------------------------------------------
init_per_suite(Config) ->
  ok = application:start(auction_server),
  Config.

end_per_suite(_Config) ->
  application:stop(auction_server),
  ok.

%%% group setup & tear down ---------------------------------------------------
init_per_group(race_integ, Config) ->
  BidderName1 = "elon musk",
  BidderName2 = "jeff bezos",
  {ok, AuctionId1} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId1),
  AuctionItems = 
    [{"book", "fiction", 1}, {"hat", "blue cap", 0}],
  {ok, [{_, "hat"}, {ItemId1, "book"}]} = 
    auction_data:add_items(AuctionId1, AuctionItems),
  [{auctionid, AuctionId1} |
    [{itemid, ItemId1} | 
      [{bidder_names, [BidderName1, BidderName2]} | 
        Config]]];
init_per_group(_, Config) ->
  Config.

end_per_group(race_integ, Config) ->
  AuctionId = ?config(auctionid, Config),
  ok = auction_data:remove_auction(AuctionId);
end_per_group(_, _Config) ->
  ok.

%%% testcase setup ------------------------------------------------------------
init_per_testcase(race_bidder_1, Config) ->
  Config;
init_per_testcase(race_bidder_2, Config) ->
  Config;
init_per_testcase(auction_house, Config) ->
  Config.

%%% testcase teardown ---------------------------------------------------------
end_per_testcase(race_bidder_1, _Config) ->
  ok;
end_per_testcase(race_bidder_2, _Config) ->
  ok;
end_per_testcase(auction_house, Config) ->
  Config.

%%% integration tests ---------------------------------------------------------

race_bidder_1(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  AuctionId1 = ?config(auctionid, Config),
  ItemId1 = ?config(itemid, Config),
  {ok, _} = auction_client_server:start_link(BidderName1),
  {ok, _} = auction_client_server:subscribe(BidderName1, AuctionId1),
  timer:sleep(2000),

  {ok, leading} = 
    auction_client_server:bid(BidderName1, AuctionId1, ItemId1, 5),

  ok = auction_client_server:stop(BidderName1),
  ok = ct:capture_stop().

auction_house(Config) ->
  AuctionId1 = ?config(auctionid, Config),
  timer:sleep(1000), % need to pause to make sure race_bidder_2 has subscribed
  % 1s 
  {ok, AuctionPid} = auction:start_link(AuctionId1),
  unlink(AuctionPid), % auction will not die even though auction_house does
  timer:sleep(2000).
  % supervisor:terminate_child(?MODULE, AuctionPid).

race_bidder_2(Config) ->
  [_, BidderName2] = ?config(bidder_names, Config),
  AuctionId1 = ?config(auctionid, Config),
  ItemId1 = ?config(itemid, Config),
  {ok, _} = auction_client_server:start_link(BidderName2),
  {ok, _} = auction_client_server:subscribe(BidderName2, AuctionId1),
  timer:sleep(2000),
  
  {ok, {not_leading, 5}} = 
    auction_client_server:bid(BidderName2, AuctionId1, ItemId1, 5),
    
  ok = auction_client_server:stop(BidderName2),
  ok = ct:capture_stop().
