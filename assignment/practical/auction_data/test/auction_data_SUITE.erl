-module(auction_data_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_create_auction/1, 
         test_add_items/1, 
         test_get_auctions/1]).

all () ->
  [test_create_auction, test_add_items, test_get_auctions].

% init test cases
init_per_testcase(_, Config) ->
  {ok, Tid} = auction_data:create_auction(),
  [{table, Tid} | Config].

% end test cases
end_per_testcase(_, Config) ->
  ets:delete(?config(table, Config)).

% tests
test_create_auction(Config) ->
  Tid = ?config(table, Config),
  undefined /= ets:info(Tid).

test_add_items(Config) ->
  Tid = ?config(table, Config),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  % test_add_items works for list of items
  {ok, [{ItemId1, "book"}, {ItemId2, "hat"}]} = 
    auction_data:add_items(Tid, AuctionItems),
  % and it returns the correct error if the Tid is invalid
  InvalidTid = 1000,
  {error, unknown_auction} = auction_data:add_items(InvalidTid, AuctionItems).

test_get_auctions(Config) ->
  Tid1 = ?config(table, Config),
  {ok, Tid2} = auction_data:create_auction(),
  {ok, [Tid1, Tid2]} = auction_data:get_auctions().