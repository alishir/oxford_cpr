-module(auction_data_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2, 
         end_per_testcase/2]).
-export([test_create_auction/1
        %  test_add_items/1, 
        %  test_get_auctions/1
]).

all() ->
  [test_create_auction
  %  test_add_items, 
  %  test_get_auctions
  ].

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

% testcase setup & tear down
init_per_testcase(_, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  [{table, AuctionId} | Config].

end_per_testcase(_, _Config) ->
  ok.

% tests
test_create_auction(_Config) ->
  {ok, AuctionId} = auction_data:create_auction().
  
test_add_items(Config) ->
  AuctionId = ?config(table, Config),
  AuctionItems = [{"book", "fiction", 0}, {"hat", "blue cap", 1}],
  % test_add_items works for list of items
  {ok, [{ItemId1, "book"}, {ItemId2, "hat"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  % and it returns the correct error if the AuctionId is invalid
  InvalidAuctionId = 1000,
  {error, unknown_auction} = auction_data:add_items(InvalidAuctionId, AuctionItems).

% test_get_auctions(Config) ->
%   AuctionId1 = ?config(table, Config),
%   {ok, AuctionId2} = auction_data:create_auction(),
%   {ok, [AuctionId1, AuctionId2]} = auction_data:get_auctions().