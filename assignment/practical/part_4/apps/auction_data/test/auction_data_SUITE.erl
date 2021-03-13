%%%----------------------------------------------------------------------------
%% File: auction_data_SUITE.erl
%% @author Nicholas Drake
%% @doc auction_data_SUITE
%% @end
%%%----------------------------------------------------------------------------
-module(auction_data_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2, 
         end_per_testcase/2]).
-export([test_create_auction/1,
         test_add_items/1,
         test_add_winning_bidder/1,
         test_get_auctions/1,
         test_get_items/1,
         test_get_items_and_lock_auction/1,
         test_get_item/1,
         test_get_winning_bidder/1,
         test_remove_auction/1,
         test_remove_item/1]).

all() ->
  [test_create_auction,
   test_add_items,
   test_add_winning_bidder,
   test_get_auctions,
   test_get_items,
   test_get_items_and_lock_auction,
   test_get_item,
   test_get_winning_bidder,
   test_remove_auction,
   test_remove_item].

%%% suite setup & tear down ---------------------------------------------------
init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  io:format("priv_directory ~s~n", [Priv]),
  application:load(mnesia),
  application:set_env(mnesia, dir, Priv),
  application:load(auction_data),
  auction_data:install([node()]),
  application:start(mnesia),
  application:start(auction_data),
  Config.

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.

%%% testcase setup ------------------------------------------------------------
init_per_testcase(test_create_auction, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  [{auction, AuctionId} |
   [{response, {ok, AuctionId}} | Config]];
init_per_testcase(test_add_winning_bidder, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  [{auction, AuctionId} | [{itemids, [ItemId1, ItemId2]} | Config]];
init_per_testcase(test_get_auctions, Config) ->
  {ok, AuctionId1} = auction_data:create_auction(),
  {ok, AuctionId2} = auction_data:create_auction(),
  [{auction, [AuctionId1, AuctionId2]} | Config];
init_per_testcase(test_get_items, Config) ->
  {ok, AuctionId1} = auction_data:create_auction(),
  {ok, AuctionId2} = auction_data:create_auction(),
  Auction1Items = [{"book", "fiction", 0}, 
                  {"hat", "blue cap", 1}, 
                  {"plate", "ceramic", 3}],
  {ok, [{ItemId1, "plate"}, {ItemId2, "hat"}, {ItemId3, "book"}]} = 
    auction_data:add_items(AuctionId1, Auction1Items),
  Auction2Items = [{"phone", "black", 0}],
  {ok, [{_, "phone"}]} = 
    auction_data:add_items(AuctionId2, Auction2Items),
  [{auction, [AuctionId1, AuctionId2]} | 
    [{itemids, [ItemId1, ItemId2, ItemId3]} | Config]];
init_per_testcase(test_get_items_and_lock_auction, Config) ->
  {ok, AuctionId1} = auction_data:create_auction(),
  {ok, AuctionId2} = auction_data:create_auction(),
  Auction1Items = [{"book", "fiction", 0}, 
                  {"hat", "blue cap", 1}, 
                  {"plate", "ceramic", 3}],
  {ok, [{ItemId1, "plate"}, {ItemId2, "hat"}, {ItemId3, "book"}]} = 
    auction_data:add_items(AuctionId1, Auction1Items),
  Auction2Items = [{"phone", "black", 0}],
  {ok, [{_, "phone"}]} = 
    auction_data:add_items(AuctionId2, Auction2Items),
  [{auction, [AuctionId1, AuctionId2]} | 
    [{itemids, [ItemId1, ItemId2, ItemId3]} | Config]];
init_per_testcase(test_get_item, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  [{auction, AuctionId} | [{itemids, [ItemId1, ItemId2]} | Config]];
init_per_testcase(test_get_winning_bidder, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  WinningBid = 3,
  WinningBidder = {"elon musk", make_ref()},
  ok = auction_data:add_winning_bidder(
    AuctionId, ItemId1, WinningBid, WinningBidder),
  [{winning, {WinningBidder, WinningBid}} |
    [{auction, AuctionId} | 
      [{itemids, [ItemId1, ItemId2]} | 
        Config]]];
init_per_testcase(test_remove_item, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  [{auction, AuctionId} | [{itemids, [ItemId1, ItemId2]} | Config]];
init_per_testcase(_, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  [{auction, AuctionId} | Config].

%%% testcase tear down --------------------------------------------------------
end_per_testcase(test_get_auctions, Config) ->
  [AuctionId1, AuctionId2] = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId1),
  ok = auction_data:remove_auction(AuctionId2);
end_per_testcase(test_get_items, Config) ->
  [AuctionId1, AuctionId2] = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId1), 
  ok = auction_data:remove_auction(AuctionId2);
end_per_testcase(test_get_items_and_lock_auction, Config) ->
  [AuctionId1, AuctionId2] = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId1), 
  ok = auction_data:remove_auction(AuctionId2);
end_per_testcase(test_remove_auction, _Config) ->
  ok;
end_per_testcase(_, Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId).

%%% tests ---------------------------------------------------------------------
test_create_auction(Config) ->
  {ok, _} = ?config(response, Config).
  
test_add_items(Config) ->
  AuctionId = ?config(auction, Config),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  % test_add_items works for list of items
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  {ok, GottenItems} = auction_data:get_items(AuctionId),
  GottenItems = lists:sort([ItemId1, ItemId2]),
  % and it returns the correct error if the AuctionId is invalid
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = 
    auction_data:add_items(InvalidAuctionId, AuctionItems).

test_add_winning_bidder(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  WinningBid = 4,
  WinningBidder = {"tom brady", make_ref()},
  ok = auction_data:add_winning_bidder(
    AuctionId, ItemId1, WinningBid, WinningBidder),
  {ok, {WinningBid, WinningBidder}} = 
    auction_data:get_winning_bidder(AuctionId, ItemId1),
  {ok, {undefined, undefined}} = 
    auction_data:get_winning_bidder(AuctionId, ItemId2),
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = 
    auction_data:add_winning_bidder(
      InvalidAuctionId, ItemId1, WinningBid, WinningBidder),
  InvalidItemId = {node(), erlang:monotonic_time(), make_ref()},
    {error, unknown_item} = auction_data:add_winning_bidder(
      AuctionId, InvalidItemId, WinningBid, WinningBidder).

test_get_auctions(Config) ->
  Auctions = ?config(auction, Config),
  {ok, AuctionIdList} = auction_data:get_auctions(),
  SortedConfigAuctions = lists:sort(Auctions),
  SortedConfigAuctions = lists:sort(AuctionIdList).

test_get_items(Config) ->
  [AuctionId1, _] = ?config(auction, Config),
  ItemIds = ?config(itemids, Config),
  {ok, GottenItems} = auction_data:get_items(AuctionId1),
  ItemIdsSorted = lists:sort(ItemIds),
  ItemIdsSorted = GottenItems,
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = auction_data:get_items(InvalidAuctionId).

test_get_items_and_lock_auction(Config) ->
  [AuctionId1, _] = ?config(auction, Config),
  ItemIds = ?config(itemids, Config),
  {ok, GottenItems} = auction_data:get_items_and_lock_auction(AuctionId1),
  ItemIdsSorted = lists:sort(ItemIds),
  ItemIdsSorted = GottenItems,
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = 
    auction_data:get_items_and_lock_auction(InvalidAuctionId),
  % shouldn't be able to add any more items now AuctionId1 is locked
  {error, unknown_auction} = 
    auction_data:add_items(AuctionId1, [{"phone", "black", 0}]).

test_get_item(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  {ok, {ItemId1, "blue cap", 1}} = auction_data:get_item(AuctionId, ItemId1),
  {ok, {ItemId2, "fiction", 0}} = auction_data:get_item(AuctionId, ItemId2),
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = auction_data:get_item(InvalidAuctionId, ItemId1),
  InvalidItemId = {node(), erlang:monotonic_time(), make_ref()},
  {error, unknown_item} = auction_data:get_item(AuctionId, InvalidItemId).

test_get_winning_bidder(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  {WinningBidder, WinningBid} = ?config(winning, Config),
  {ok, {WinningBid, WinningBidder}} = 
    auction_data:get_winning_bidder(AuctionId, ItemId1),
  {ok, {undefined, undefined}} = 
    auction_data:get_winning_bidder(AuctionId, ItemId2),
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = 
    auction_data:get_winning_bidder(InvalidAuctionId, ItemId1),
  InvalidItemId = {node(), erlang:monotonic_time(), make_ref()},
  {error, unknown_item} = 
    auction_data:get_winning_bidder(AuctionId, InvalidItemId).

test_remove_auction(Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId),
  {ok, []} = auction_data:get_auctions().

test_remove_item(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  {ok, {ItemId1, "blue cap", 1}} = auction_data:get_item(AuctionId, ItemId1),
  {ok, {ItemId2, "fiction", 0}} = auction_data:get_item(AuctionId, ItemId2),
  ok = auction_data:remove_item(AuctionId, ItemId1),
  {error, unknown_item} = auction_data:get_item(AuctionId, ItemId1),
  {ok, {ItemId2, "fiction", 0}} = auction_data:get_item(AuctionId, ItemId2),
  {error, unknown_item} = auction_data:remove_item(AuctionId, ItemId1),
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = 
    auction_data:remove_item(InvalidAuctionId, ItemId2).
