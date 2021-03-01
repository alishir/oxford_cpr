-module(auction_SUITE).

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
         test_get_starting_bid/1,
         test_add_winning_bidder/1,
         test_init/1,
         test_auction_item/1,
         test_auction_ended/1,
         test_check_for_invalid_bid/1,
         test_check_leading_bid/1,
         test_get_next_itemid/1]).

all() ->
  [{group, auction_data_dep},
   {group, statem_dep}].

groups() -> 
  [{auction_data_dep, [], [test_start_link,
                           test_get_starting_bid,
                           test_add_winning_bidder]},
   {statem_dep, [], [test_init,
                     test_auction_item,
                     test_auction_ended,
                     test_check_for_invalid_bid,
                     test_check_leading_bid,
                     test_get_next_itemid]}].

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

% group setup & tear down
init_per_group(auction_data_dep, Config) ->
  Config;
init_per_group(statem_dep, Config) ->
  AuctionId = make_ref(),
  ItemId1 = {erlang:monotonic_time(), make_ref()},
  ItemId2 = {erlang:monotonic_time(), make_ref()},
  M = #{auction_id => AuctionId, 
        current_itemid => ItemId1,
        remaining_itemids => ItemId2, 
        auctioned_itemids => [],
        starting_bid => undefined,
        leading_bid => undefined,
        leading_bidder => undefined},
  [{state_map, M} | 
    [{auction, AuctionId} | 
      [{itemids, [ItemId1, ItemId2]} | 
        Config]]].

end_per_group(auction_data_dep, _Config) ->
  ok;
end_per_group(statem_dep, _Config) ->
  ok.

% testcase setup
init_per_testcase(test_start_link, Config) ->
  {ok, AuctionId1} = auction_data:create_auction(),
  {ok, AuctionId2} = auction_data:create_auction(),
  AuctionItems = 
    [{"book", "fiction", 1}, {"hat", "blue cap", 0}],
  {ok, [{_, "hat"}, {_, "book"}]} = 
    auction_data:add_items(AuctionId1, AuctionItems),
  [{auction, [AuctionId1, AuctionId2]} | Config];
init_per_testcase(test_get_starting_bid, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  StartingBid1 = 0,
  StartingBid2 = 1,
  AuctionItems = 
    [{"book", "fiction", StartingBid2}, {"hat", "blue cap", StartingBid1}],
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  [{starting_bids, [StartingBid1, StartingBid2]} |
    [{auction, AuctionId} | 
      [{itemids, [ItemId1, ItemId2]} | 
        Config]]];
init_per_testcase(test_add_winning_bidder, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  StartingBid1 = 0,
  StartingBid2 = 1,
  AuctionItems = 
    [{"book", "fiction", StartingBid2}, {"hat", "blue cap", StartingBid1}],
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  [{starting_bids, [StartingBid1, StartingBid2]} |
    [{auction, AuctionId} | 
      [{itemids, [ItemId1, ItemId2]} | 
        Config]]];
init_per_testcase(test_check_for_invalid_bid, Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  NewStartingBid = 7,
  Input1 = #{auction_id => AuctionId, 
             current_itemid => ItemId2,
             remaining_itemids => [], 
             auctioned_itemids => [ItemId1],
             starting_bid => NewStartingBid,
             leading_bid => undefined,
             leading_bidder => undefined},
  Input2 = #{auction_id => AuctionId, 
             current_itemid => ItemId1,
             remaining_itemids => [ItemId2], 
             auctioned_itemids => [],
             starting_bid => NewStartingBid,
             leading_bid => undefined,
             leading_bidder => undefined},
  [{response_states, [Input1, Input2]} | Config];
init_per_testcase(test_check_leading_bid, Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  NewStartingBid = 3,
  LeadingBid = 5,
  LeadingBidder = {"jeff bezos", make_ref()},
  M1 = #{auction_id => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => NewStartingBid,
         leading_bid => undefined,
         leading_bidder => undefined},
  M2 = #{auction_id => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => NewStartingBid,
         leading_bid => LeadingBid,
         leading_bidder => LeadingBidder},
  [{leading_bid, LeadingBid} |
    [{starting_bid , NewStartingBid} | 
      [{input_states, [M1, M2]} | Config]]];
init_per_testcase(_, Config) ->
  Config.

% testcase tear down
end_per_testcase(test_start_link, Config) ->
  [AuctionId1, AuctionId2] = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId1),
  ok = auction_data:remove_auction(AuctionId2);
end_per_testcase(test_get_starting_bid, Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId);
end_per_testcase(test_add_winning_bidder, Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId);
end_per_testcase(_, Config) ->
  ok.

% auction_data_dep tests
test_start_link(Config) ->
  [AuctionId1, AuctionId2] = ?config(auction, Config),
  {ok, _} = auction:start_link(AuctionId1),
  % auction still exists but no items so unknown_auction
  {error, unknown_auction} = auction:start_link(AuctionId2),
  InvalidAuctionId = make_ref(),
  {error, unknown_auction} = auction:start_link(InvalidAuctionId).

test_get_starting_bid(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  [StartingBid1, StartingBid2] = ?config(starting_bids, Config),
  UndefinedStartingBid = undefined,
  StartingBid1 = 
    auction:get_starting_bid(AuctionId, ItemId1, UndefinedStartingBid),
  StartingBid2 = 
    auction:get_starting_bid(AuctionId, ItemId2, UndefinedStartingBid),
  AlreadyHaveStartingBid = 3,
  AlreadyHaveStartingBid = 
    auction:get_starting_bid(AuctionId, ItemId1, AlreadyHaveStartingBid).

test_add_winning_bidder(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  LeadingBid = 3,
  LeadingBidder = {"elon musk", make_ref()},
  ok = auction:add_winning_bidder(AuctionId, ItemId1, LeadingBid, LeadingBidder),
  {ok, {LeadingBid, LeadingBidder}} = 
    auction_data:get_winning_bidder(AuctionId, ItemId1),
  ok = auction:add_winning_bidder(AuctionId, ItemId2, undefined, undefined),
  {ok, {undefined, undefined}} = 
    auction_data:get_winning_bidder(AuctionId, ItemId2).

% statem_dep tests
test_auction_item(Config) ->
  ok.

test_init(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  M = ?config(state_map, Config),
  {ok, 
   auction_item,
   N,
   [{state_timeout, 10000, next_item}]} = 
    auction:init([AuctionId, ItemId1, [ItemId2]]),
  ?_assertEqual(M, N).

test_auction_ended(Config) ->
  ok.

test_check_for_invalid_bid(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  M = ?config(state_map, Config),
  From = from,
  [M1, M2] = ?config(response_states, Config),
  % check if item_already_sold
  {keep_state,
   N1,
   [{reply, From, {error, item_already_sold}}]} = 
    auction:check_for_invalid_bid(M, 
                                  5, 
                                  From, 
                                  AuctionId, 
                                  AuctionId, 
                                  ItemId2, 
                                  ItemId1, 
                                  [ItemId1]),
  ?_assertEqual(M1, N1),
  % check if invalid_auction
  InvalidAuctionId = make_ref(),
  {keep_state,
   N2,
   [{reply, From, {error, invalid_auction}}]} = 
    auction:check_for_invalid_bid(M, 
                                  5, 
                                  From, 
                                  AuctionId, 
                                  InvalidAuctionId, 
                                  ItemId1, 
                                  ItemId1, 
                                  []),
  ?_assertEqual(M2, N2),
  % check if invalid_item
  {keep_state,
   N3,
   [{reply, From, {error, invalid_item}}]} = 
    auction:check_for_invalid_bid(M, 
                                  5, 
                                  From, 
                                  AuctionId, 
                                  AuctionId, 
                                  ItemId1, 
                                  ItemId2, 
                                  []),
  ?_assertEqual(M2, N3),
  % if all valid then should be undefined
  undefined = 
    auction:check_for_invalid_bid(M, 
                                  5, 
                                  From, 
                                  AuctionId, 
                                  AuctionId, 
                                  ItemId1, 
                                  ItemId1, 
                                  []),
  undefined = 
    auction:check_for_invalid_bid(M, 
                                  5, 
                                  From, 
                                  AuctionId, 
                                  AuctionId, 
                                  ItemId2, 
                                  ItemId2, 
                                  [ItemId1]).

test_check_leading_bid(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  [M_no_leading, M_leading] = ?config(input_states, Config),
  StartingBid = ?config(starting_bid, Config),
  LeadingBid = ?config(leading_bid, Config),
  Bidder = {"elon musk", make_ref()},
  From = from,
  % no leading, bid < starting
  E1 = M_no_leading,
  {keep_state,
   N1,
   [{reply, 
     From, 
     {ok, {non_leading, StartingBid}}}]} = 
    auction:check_leading_bid(M_no_leading,
                              From,
                              StartingBid - 1,
                              Bidder,
                              StartingBid, 
                              undefined),
  ?_assertEqual(E1, N1),
  % no leading, bid >= starting
  E2 = #{auction_id => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => StartingBid,
         leading_bid => StartingBid,
         leading_bidder => Bidder}, 
  {next_state,
   auction_item,
   N2,
   [{reply, From, {ok, leading}},
    {state_timeout, 10000, next_item}]} = 
    auction:check_leading_bid(M_no_leading,
                              From,
                              StartingBid,
                              Bidder,
                              StartingBid, 
                              undefined),
  ?_assertEqual(E2, N2).

test_get_next_itemid(Config) ->
  ok.