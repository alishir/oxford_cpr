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
         test_get_auctions/1,
         test_get_items/1,
         test_get_item/1,
         test_remove_auction/1]).

all() ->
  [test_create_auction,
   test_add_items,
   test_get_auctions,
   test_get_items,
   test_get_item,
   test_remove_auction].

% suite setup & tear down
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

% testcase setup
init_per_testcase(test_create_auction, Config) ->
  Response = auction_data:create_auction(),
  [{response, Response} | Config];
init_per_testcase(test_get_auctions, Config) ->
  {ok, AuctionId1} = auction_data:create_auction(),
  {ok, AuctionId2} = auction_data:create_auction(),
  [{auctions, [AuctionId1, AuctionId2]} | Config];
init_per_testcase(test_get_items, Config) ->
  {ok, AuctionId1} = auction_data:create_auction(),
  {ok, AuctionId2} = auction_data:create_auction(),
  Auction1Items = [{"book", "fiction", 0}, 
                  {"hat", "blue cap", 1}, 
                  {"plate", "ceramic", 3}],
  {ok, [{ItemId1, "plate"}, {ItemId2, "hat"}, {ItemId3, "book"}]} = 
    auction_data:add_items(AuctionId1, Auction1Items),
  Auction2Items = [{"phone", "black", 0}],
  {ok, [{ItemId4, "phone"}]} = 
    auction_data:add_items(AuctionId2, Auction2Items),
  [{auction, AuctionId1} | [{itemids, [ItemId1, ItemId2, ItemId3]} | Config]];
init_per_testcase(test_get_item, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  [{auction, AuctionId} | [{itemids, [ItemId1, ItemId2]} | Config]];
init_per_testcase(_, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  [{auction, AuctionId} | Config].

% testcase tear down
end_per_testcase(_, Config) ->
  AuctionId = ?config(auction, Config),
  auction_data:remove_auction(AuctionId).

% tests
test_create_auction(Config) ->
  {ok, AuctionId} = ?config(response, Config).
  
test_add_items(Config) ->
  AuctionId = ?config(auction, Config),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  % test_add_items works for list of items
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  % and it returns the correct error if the AuctionId is invalid
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = auction_data:add_items(InvalidAuctionId, AuctionItems).

test_get_auctions(Config) ->
  Auctions = ?config(auctions, Config),
  {ok, AuctionIdList} = auction_data:get_auctions(),
  lists:sort(Auctions) =:= lists:sort(AuctionIdList).

test_get_items(Config) ->
  AuctionId = ?config(auction, Config),
  ItemIds = ?config(itemids, Config),
  GottenItems = auction_data:get_items(AuctionId),
  ItemIdsSorted = lists:sort(ItemIds),
  ItemIdsSorted = GottenItems,
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = auction_data:get_items(InvalidAuctionId).

test_get_item(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  {ok, {"hat", "blue cap", 1}} = auction_data:get_item(AuctionId, ItemId1),
  {ok, {"book", "fiction", 0}} = auction_data:get_item(AuctionId, ItemId2),
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = auction_data:get_item(InvalidAuctionId, ItemId1),
  InvalidItemId = {erlang:monotonic_time(), make_ref()},
  {error, unknown_item} = auction_data:get_item(AuctionId, InvalidItemId).

test_remove_auction(Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId).