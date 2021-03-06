%%%----------------------------------------------------------------------------
%% File: auction_client_server_SUITE.erl
%% @author Nicholas Drake
%% @doc auction_client_server_SUITE
%% @end
%%%----------------------------------------------------------------------------
-module(auction_client_server_SUITE).

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
-export([test_start_link/1,
         test_stop/1,
         test_get_auctions/1,
         test_subscribe/1,
         test_unsubscribe/1,
         test_bid/1,
         test_auction_messages/1]).

-export([test_add_automated_bid_to_max/1,
         other_bidder/1, 
         auction_house/1]).

all() -> 
  [test_start_link,
   test_stop,
   test_get_auctions,
   test_subscribe,
   test_unsubscribe,
   test_bid,
   test_auction_messages,
   {group, automated_integ}].

groups() ->
  [{automated_integ, 
    [], 
    [{group, automated_integ_components}]},
   {automated_integ_components, 
    [parallel], 
    [test_add_automated_bid_to_max, auction_house, other_bidder]}].

%%% suite setup & tear down ---------------------------------------------------
init_per_suite(Config) ->
  ok = application:start(auction_server),
  Config.

end_per_suite(_Config) ->
  application:stop(auction_server),
  ok.

%%% group setup & tear down ---------------------------------------------------
init_per_group(automated_integ, Config) ->
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

end_per_group(automated_integ, Config) ->
  AuctionId = ?config(auctionid, Config),
  ok = auction_data:remove_auction(AuctionId);
end_per_group(_, _Config) ->
  ok.

%%% testcase setup ------------------------------------------------------------
init_per_testcase(test_start_link, Config) ->
  BidderName1 = "elon musk",
  BidderName2 = "jeff bezos",
  [{bidder_names, [BidderName1, BidderName2]} | Config];
init_per_testcase(test_add_automated_bid_to_max, Config) ->
  Config;
init_per_testcase(other_bidder, Config) ->
  Config;
init_per_testcase(auction_house, Config) ->
  Config;
init_per_testcase(_, Config) ->
  BidderName1 = "elon musk",
  BidderName2 = "jeff bezos",
  {ok, ClientPid1} = auction_client_server:start_link(BidderName1),
  {ok, ClientPid2} = auction_client_server:start_link(BidderName2),
  [{bidder_names, [BidderName1, BidderName2]} | 
    [{client_pids, [ClientPid1, ClientPid2]} | 
      Config]].

%%% testcase teardown ---------------------------------------------------------
end_per_testcase(test_start_link, _Config) ->
  ok;
end_per_testcase(test_add_automated_bid_to_max, _Config) ->
  ok;
end_per_testcase(other_bidder, _Config) ->
  ok;
end_per_testcase(auction_house, Config) ->
  Config;
end_per_testcase(test_stop, Config) ->
  [_, BidderName2] = ?config(bidder_names, Config),
  ok = auction_client_server:stop(BidderName2);
end_per_testcase(_, Config) ->
  [BidderName1, BidderName2] = ?config(bidder_names, Config),
  ok = auction_client_server:stop(BidderName1),
  ok = auction_client_server:stop(BidderName2).

%%% unit tests ----------------------------------------------------------------
test_start_link(Config) ->
  [BidderName1, BidderName2] = ?config(bidder_names, Config),
  {ok, ClientPid1} = auction_client_server:start_link(BidderName1),
  {ok, ClientPid2} = auction_client_server:start_link(BidderName2),
  true = (ClientPid1 =/= ClientPid2).

test_stop(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  [ClientPid1, ClientPid2] = ?config(client_pids, Config),
  ok = auction_client_server:stop(BidderName1),
  false = is_process_alive(ClientPid1),
  true = is_process_alive(ClientPid2).

test_get_auctions(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  ok = ct:capture_start(),

  [] = auction_client_server:get_auctions(BidderName1),
  timer:sleep(100),
  ExpectedString1 = ["List of auctions: []\n"],
  ExpectedString1 = ct:capture_get(),

  {ok, AuctionId1} = auction_data:create_auction(),
  [AuctionId1] = auction_client_server:get_auctions(BidderName1),
  timer:sleep(100),
  ExpectedString2 = lists:flatten("List of auctions: [" ++ 
    io_lib:format("~p", [AuctionId1]) ++ "]\n"),
  [ExpectedString2] = ct:capture_get(),

  {ok, AuctionId2} = auction_data:create_auction(),
  [_, _] = auction_client_server:get_auctions(BidderName1),
  timer:sleep(100),
  ExpectedString3 = lists:flatten("List of auctions: [" ++ 
    io_lib:format("~p,~n                   ~p", [AuctionId1, AuctionId2]) ++ 
    "]\n"),
  ExpectedString4 = lists:flatten("List of auctions: [" ++ 
    io_lib:format("~p,~n                   ~p", [AuctionId2, AuctionId1]) ++ 
    "]\n"),
  Captured = ct:capture_get(),
  true = (([ExpectedString3] =:= Captured) or 
          ([ExpectedString4] =:= Captured)),

  ok = auction_data:remove_auction(AuctionId1),
  ok = auction_data:remove_auction(AuctionId2),
  ok = ct:capture_stop().

test_subscribe(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  ok = ct:capture_start(),
  {ok, AuctionId1} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId1),

  {ok, _} = auction_client_server:subscribe(BidderName1, AuctionId1),
  ExpectedString = lists:flatten(
    io_lib:format("AuctionId ~p: Subscribed\n", [AuctionId1])),
  [ExpectedString] = ct:capture_get(),  

  ok = auction_data:remove_auction(AuctionId1),
  ok = ct:capture_stop().

test_unsubscribe(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  {ok, AuctionId1} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId1),
  {ok, _} = auction_client_server:subscribe(BidderName1, AuctionId1),
  ok = ct:capture_start(),

  ok = auction_client_server:unsubscribe(BidderName1, AuctionId1),
  ExpectedString = lists:flatten(
    io_lib:format("AuctionId ~p: Unsubscribed\n", [AuctionId1])),
  [ExpectedString] = ct:capture_get(),  

  ok = auction_data:remove_auction(AuctionId1),
  ok = ct:capture_stop().

test_bid(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  {ok, AuctionId1} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId1),
  AuctionItems = 
    [{"book", "fiction", 1}, {"hat", "blue cap", 0}],
  {ok, [{_, "hat"}, {ItemId1, "book"}]} = 
    auction_data:add_items(AuctionId1, AuctionItems),
  {ok, _} = auction_client_server:subscribe(BidderName1, AuctionId1),
  {ok, _AuctionPid} = auction:start_link(AuctionId1),
  timer:sleep(100),
  
  ok = ct:capture_start(),
  {ok, leading} = 
    auction_client_server:bid(BidderName1, AuctionId1, ItemId1, 5),
  ExpectedString1 = lists:flatten(
    io_lib:format("AuctionId ~p: Submitted bid ~p\n", [AuctionId1, 5])),
  [ExpectedString1] = ct:capture_get(),  

  ok = auction_data:remove_auction(AuctionId1),
  ok = ct:capture_stop().

test_auction_messages(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  {ok, AuctionId1} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId1),
  AuctionItems = 
    [{"book", "fiction", 1}, {"hat", "blue cap", 0}],
  {ok, [{_, "hat"}, {ItemId1, "book"}]} = 
    auction_data:add_items(AuctionId1, AuctionItems),
  {ok, _} = auction_client_server:subscribe(BidderName1, AuctionId1),
  
  ok = ct:capture_start(),
  {ok, _AuctionPid} = auction:start_link(AuctionId1),
  ExpectedString1 = lists:flatten(
    io_lib:format("AuctionId ~p: Started\n", [AuctionId1])),
  ExpectedString2 = lists:flatten(
  io_lib:format("AuctionId ~p: New item fiction with starting bid ~p\n", 
    [AuctionId1, 1])),
  timer:sleep(100),
  SortedExpectedStrings = lists:sort([ExpectedString1, ExpectedString2]),
  SortedExpectedStrings = lists:sort(ct:capture_get()),

  timer:sleep(2000),
  {ok, leading} = 
    auction_client_server:bid(BidderName1, AuctionId1, ItemId1, 5),
  ExpectedString3 = lists:flatten(
    io_lib:format("AuctionId ~p: Submitted bid ~p\n", [AuctionId1, 5])),
  ExpectedString4 = lists:flatten(
    io_lib:format("AuctionId ~p: Bid ~p\n", [AuctionId1, 5])),
  timer:sleep(100),
  [ExpectedString3, ExpectedString4] = ct:capture_get(),  

  timer:sleep(11000),
  ExpectedString5 = lists:flatten(
    io_lib:format("AuctionId ~p: Item sold. Winning bid ~p\n", 
    [AuctionId1, 5])),
  ExpectedString6 = lists:flatten(
      io_lib:format("AuctionId ~p: New item blue cap with starting bid ~p\n", 
      [AuctionId1, 0])),
  [ExpectedString5, ExpectedString6] = ct:capture_get(),  
  
  timer:sleep(11000),
  ExpectedString7 = lists:flatten(
    io_lib:format("AuctionId ~p: Closed\n", [AuctionId1])),
  [ExpectedString7] = ct:capture_get(),  

  ok = auction_data:remove_auction(AuctionId1),
  ok = ct:capture_stop().

test_add_automated_bid_to_max(Config) ->
  [BidderName1, _] = ?config(bidder_names, Config),
  AuctionId1 = ?config(auctionid, Config),
  ItemId1 = ?config(itemid, Config),
  {ok, _} = auction_client_server:start_link(BidderName1),
  {ok, _} = auction_client_server:subscribe(BidderName1, AuctionId1),
  timer:sleep(2000),  % need to pause to avoid capturing start_link messages
  % 2s
  ok = ct:capture_start(),
  {ok, leading} = auction_client_server:add_automated_bid_to_max(
    BidderName1, AuctionId1, ItemId1, 3, 10),
  ExpectedString1 = lists:flatten(  
    io_lib:format("AuctionId ~p: Added automatic bid.\n", [AuctionId1])),
  ExpectedString2 = lists:flatten(  
    io_lib:format("              Start bid: ~p, Max bid: ~p\n", [3, 10])),
  ExpectedString3 = lists:flatten(  
    io_lib:format("AuctionId ~p: Submitted bid ~p\n", [AuctionId1, 3])),
  ExpectedString4 = lists:flatten(
    io_lib:format("AuctionId ~p: Bid ~p\n", [AuctionId1, 3])),
  timer:sleep(100), % need to pause to make sure all the messages arrive
  [ExpectedString1, ExpectedString2, ExpectedString3, ExpectedString4] = 
    ct:capture_get(),  
  ct:print(ExpectedString1),
  ct:print(ExpectedString2),
  ct:print(ExpectedString3),
  ct:print(ExpectedString4),

  timer:sleep(2000),
  % 4s
  ExpectedString5 = lists:flatten(
    io_lib:format("AuctionId ~p: Bid ~p\n", [AuctionId1, 5])),
  ExpectedString6 = lists:flatten(  
    io_lib:format("AuctionId ~p: Submitted bid ~p\n", [AuctionId1, 6])),
  ExpectedString7 = lists:flatten(
    io_lib:format("AuctionId ~p: Bid ~p\n", [AuctionId1, 6])),
  [ExpectedString5, ExpectedString6, ExpectedString7] = ct:capture_get(),
  ct:print(ExpectedString5),
  ct:print(ExpectedString6),
  ct:print(ExpectedString7),

  timer:sleep(2000),
  % 6s other bidder bids 10 and 10 is maximum so no counter-bid
  ExpectedString8 = lists:flatten(
    io_lib:format("AuctionId ~p: Bid ~p\n", [AuctionId1, 10])),
  [ExpectedString8] = ct:capture_get(),
  ct:print(ExpectedString8),

  ok = auction_client_server:stop(BidderName1),
  ok = ct:capture_stop().

auction_house(Config) ->
  AuctionId1 = ?config(auctionid, Config),
  timer:sleep(1000), % need to pause to make sure other_bidder has subscribed
  % 1s 
  {ok, AuctionPid} = auction:start_link(AuctionId1),
  unlink(AuctionPid), % auction will not die even though auction_house does
  timer:sleep(6000).
  % supervisor:terminate_child(?MODULE, AuctionPid).

other_bidder(Config) ->
  [_, BidderName2] = ?config(bidder_names, Config),
  AuctionId1 = ?config(auctionid, Config),
  ItemId1 = ?config(itemid, Config),
  {ok, _} = auction_client_server:start_link(BidderName2),
  {ok, _} = auction_client_server:subscribe(BidderName2, AuctionId1),
  timer:sleep(3000),
  % 3s
  {ok, leading} = 
    auction_client_server:bid(BidderName2, AuctionId1, ItemId1, 5),

  % 5s
  timer:sleep(2000),
  {ok, leading} = 
    auction_client_server:bid(BidderName2, AuctionId1, ItemId1, 10),
    
  ok = auction_client_server:stop(BidderName2),
  ok = ct:capture_stop().
