-module(auction_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-test_warnings([start/0, start_link/1, init/0, time_to_go/1]).

-export([all/0, 
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2, 
         end_per_testcase/2]).

-export([test_start_link/1,
         test_auction_item/1,
         test_get_starting_bid/1,
         test_add_winning_bidder/1,
         test_init/1,
         test_auction_ended/1,
         test_check_for_invalid_bid/1,
         test_check_leading_bid/1,
         test_get_next_itemid/1]).

all() ->
  [{group, statem_dep_unit},
   {group, auction_data_dep_unit}].

groups() -> 
  [{auction_data_dep_unit, [], [test_start_link,
                                test_auction_item,
                                test_get_starting_bid,
                                test_add_winning_bidder]},
   {statem_dep_unit, [], [test_init,
                          test_auction_ended,
                          test_check_for_invalid_bid,
                          test_check_leading_bid,
                          test_get_next_itemid]},
   {single_bidder_int, [], []}].

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

%%% group setup & tear down ---------------------------------------------------
init_per_group(auction_data_dep_unit, Config) ->
  Config;
init_per_group(statem_dep_unit, Config) ->
  AuctionId = make_ref(),
  ItemId1 = {erlang:monotonic_time(), make_ref()},
  ItemId2 = {erlang:monotonic_time(), make_ref()},
  M = #{auctionid => AuctionId, 
        current_itemid => ItemId1,
        remaining_itemids => [ItemId2], 
        auctioned_itemids => [],
        starting_bid => undefined,
        leading_bid => undefined,
        leading_bidder => undefined},
  [{state_map, M} | 
    [{auction, AuctionId} | 
      [{itemids, [ItemId1, ItemId2]} | 
        Config]]];
init_per_group(_, Config) ->
  Config.

end_per_group(_, _Config) ->
  ok.

%%% testcase setup ------------------------------------------------------------
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
  NewStartingBid = 4,
  Input1 = #{auctionid => AuctionId, 
             current_itemid => ItemId2,
             remaining_itemids => [], 
             auctioned_itemids => [ItemId1],
             starting_bid => NewStartingBid,
             leading_bid => undefined,
             leading_bidder => undefined},
  Input2 = #{auctionid => AuctionId, 
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
  M1 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => NewStartingBid,
         leading_bid => undefined,
         leading_bidder => undefined},
  M2 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => NewStartingBid,
         leading_bid => LeadingBid,
         leading_bidder => LeadingBidder},
  [{leading_bid, LeadingBid} |
    [{starting_bid , NewStartingBid} | 
      [{input_states, [M1, M2]} | Config]]];
init_per_testcase(test_auction_item, Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  StartingBid1 = 3,
  StartingBid2 = 1,
  AuctionItems = 
    [{"book", "fiction", StartingBid2}, {"hat", "blue cap", StartingBid1}],
  {ok, [{ItemId1, "hat"}, {ItemId2, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  LeadingBid = 5,
  LeadingBidder = {"jeff bezos", make_ref()},
  M1 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => StartingBid1,
         leading_bid => undefined,
         leading_bidder => undefined},
  M2 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => StartingBid1,
         leading_bid => LeadingBid,
         leading_bidder => LeadingBidder},
  [{itemids, [ItemId1, ItemId2]} | 
    [{auction, AuctionId} | 
      [{leading_bidder, LeadingBidder} |
        [{leading_bid, LeadingBid} |
          [{starting_bids , [StartingBid1, StartingBid2]} | 
            [{input_states, [M1, M2]} | Config]]]]]]; 
init_per_testcase(_, Config) ->
  Config.

%%% testcase tear down --------------------------------------------------------
end_per_testcase(test_start_link, Config) ->
  [AuctionId1, AuctionId2] = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId1),
  ok = auction_data:remove_auction(AuctionId2);
end_per_testcase(test_auction_item, Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId);
end_per_testcase(test_get_starting_bid, Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId);
end_per_testcase(test_add_winning_bidder, Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId);
end_per_testcase(_, _Config) ->
  ok.

%%% auction_data_dep_unit_unit tests -----------------------------------------------
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

% statem_dep_unit tests
test_auction_item(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  Bidder = {"elon musk", make_ref()},
  [M_no_leading, M_leading] = ?config(input_states, Config),
  From = from,
  Bid = 7,
  [StartingBid1, _] = ?config(starting_bids, Config),
  LeadingBid = ?config(leading_bid, Config),
  LeadingBidder = ?config(leading_bidder, Config),
  % test for invalid auction
  InvalidAuctionId = make_ref(),
  InvalidAuctionIdBidMessage = {bid, InvalidAuctionId, ItemId1, Bid, Bidder},
  E1 = M_no_leading,
  {keep_state,
   N1,
   [{reply, From, {error, invalid_auction}}]} = 
    auction:auction_item({call, From},
                         InvalidAuctionIdBidMessage,
                         M_no_leading),
  E1 = N1,
  % test for invalid item
  InvalidItemId = ItemId2,
  InvalidItemIdBidMessage = {bid, AuctionId, InvalidItemId, Bid, Bidder},
  E2 = M_no_leading,
  {keep_state,
   N2,
   [{reply, From, {error, invalid_item}}]} = 
    auction:auction_item({call, From},
                         InvalidItemIdBidMessage,
                         M_no_leading),
  E2 = N2,
  % test for bid < starting bid and no leading bid
  E3 = M_no_leading,
  LessThanStartingBid = 2,
  LessThanStartingBidMessage = {bid, AuctionId, ItemId1, LessThanStartingBid, Bidder},
  {keep_state,
   N3,
   [{reply, From, {ok, {not_leading, StartingBid1}}}]} = 
    auction:auction_item({call, From},
                         LessThanStartingBidMessage,
                         M_no_leading),
  E3 = N3,
  % test for bid >= starting bid and no leading bid
  MoreThanStartingBid = 4,
  MoreThanStartingBidMessage = {bid, AuctionId, ItemId1, MoreThanStartingBid, Bidder},
  E4 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => StartingBid1,
         leading_bid => MoreThanStartingBid,
         leading_bidder => Bidder}, 
  {next_state,
   auction_item,
   N4,
   [{reply, From, {ok, leading}},
    {state_timeout, 10000, next_item}]} = 
    auction:auction_item({call, From},
                         MoreThanStartingBidMessage,
                         M_no_leading),
  E4 = N4,
  % test for bid <= starting bid and leading bid
  LessThanLeadingBid = 4,
  LessThanLeadingBidMessage = {bid, AuctionId, ItemId1, LessThanLeadingBid, Bidder},
  E5 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => StartingBid1,
         leading_bid => LeadingBid,
         leading_bidder => LeadingBidder}, 
  {keep_state,
   N5,
   [{reply, From, {ok, {not_leading, LeadingBid}}}]} = 
    auction:auction_item({call, From},
                         LessThanLeadingBidMessage,
                         M_leading),
  E5 = N5,
  % test for bid <= starting bid and leading bid
  MoreThanLeadingBid = 6,
  MoreThanLeadingBidMessage = {bid, AuctionId, ItemId1, MoreThanLeadingBid, Bidder},
  E6 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => StartingBid1,
         leading_bid => MoreThanLeadingBid,
         leading_bidder => Bidder}, 
  {next_state,
   auction_item,
   N6,
   [{reply, From, {ok, leading}},
    {state_timeout, 10000, next_item}]} = 
    auction:auction_item({call, From},
                         MoreThanLeadingBidMessage,
                         M_leading),
  E6 = N6,
  % test statetimeout
  M7 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => StartingBid1,
         leading_bid => MoreThanLeadingBid,
         leading_bidder => Bidder}, 
  E7 = #{auctionid => AuctionId, 
         current_itemid => ItemId2,
         remaining_itemids => [], 
         auctioned_itemids => [ItemId1],
         starting_bid => undefined,
         leading_bid => undefined,
         leading_bidder => undefined}, 
  {next_state,
   auction_item,
   N7,
   [{state_timeout, 10000, next_item}]} 
    = auction:auction_item(state_timeout,
                           next_item,
                           M7),
  E7 = N7,
  {ok, {MoreThanLeadingBid, Bidder}} = 
    auction_data:get_winning_bidder(AuctionId, ItemId1).

test_init(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  M = ?config(state_map, Config),
  {ok, 
   auction_item,
   N,
   [{state_timeout, 10000, next_item}]} = 
    auction:init([AuctionId, ItemId1, [ItemId2]]),
  M=N.

test_auction_ended(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, _] = ?config(itemids, Config),
  Bidder = {"elon musk", make_ref()},
  M = ?config(state_map, Config),
  From = from,
  Bid = 5,
  BidMessage = {bid, AuctionId, ItemId1, Bid, Bidder},
  {keep_state,
   M,
   [{reply, From, {error, auction_ended}}]} = 
    auction:auction_ended({call, From},
                          BidMessage,
                          M).

test_check_for_invalid_bid(Config) ->
  AuctionId = ?config(auction, Config),
  [ItemId1, ItemId2] = ?config(itemids, Config),
  From = from,
  [M1, M2] = ?config(response_states, Config),
  % check if item_already_sold
  E1 = M1,
  {keep_state,
   N1,
   [{reply, From, {error, item_already_sold}}]} = 
    auction:check_for_invalid_bid(M1, 
                                  From, 
                                  AuctionId, 
                                  AuctionId, 
                                  ItemId2, 
                                  ItemId1, 
                                  [ItemId1]),
  E1 = N1,
  % check if invalid_auction
  InvalidAuctionId = make_ref(),
  E2 = M2,
  {keep_state,
   N2,
   [{reply, From, {error, invalid_auction}}]} = 
    auction:check_for_invalid_bid(M2, 
                                  From, 
                                  AuctionId, 
                                  InvalidAuctionId, 
                                  ItemId1, 
                                  ItemId1, 
                                  []),
  E2 = N2,
  % check if invalid_item
  E3 = M2,
  {keep_state,
   N3,
   [{reply, From, {error, invalid_item}}]} = 
    auction:check_for_invalid_bid(M2, 
                                  From, 
                                  AuctionId, 
                                  AuctionId, 
                                  ItemId1, 
                                  ItemId2, 
                                  []),
  E3 = N3,
  % if all valid then should be undefined
  undefined = 
    auction:check_for_invalid_bid(M2, 
                                  From, 
                                  AuctionId, 
                                  AuctionId, 
                                  ItemId1, 
                                  ItemId1, 
                                  []),
  undefined = 
    auction:check_for_invalid_bid(M2, 
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
     {ok, {not_leading, StartingBid}}}]} = 
    auction:check_leading_bid(M_no_leading,
                              From,
                              StartingBid - 1,
                              Bidder,
                              StartingBid, 
                              undefined),
  E1 = N1,
  % no leading, bid >= starting
  E2 = #{auctionid => AuctionId, 
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
  E2 = N2,
  % leading, bid > starting & <= leading
  E3 = M_leading,
  {keep_state,
   N3,
   [{reply, 
     From, 
     {ok, {not_leading, LeadingBid}}}]} = 
    auction:check_leading_bid(M_leading,
                              From,
                              StartingBid + 1,
                              Bidder,
                              StartingBid, 
                              LeadingBid),
  E3 = N3,
  % leading, bid > leading
  E4 = #{auctionid => AuctionId, 
         current_itemid => ItemId1,
         remaining_itemids => [ItemId2], 
         auctioned_itemids => [],
         starting_bid => StartingBid,
         leading_bid => LeadingBid + 1,
         leading_bidder => Bidder},
  {next_state,
   auction_item,
   N4,
   [{reply, From, {ok, leading}},
    {state_timeout, 10000, next_item}]} = 
    auction:check_leading_bid(M_leading,
                              From,
                              LeadingBid + 1,
                              Bidder,
                              StartingBid, 
                              LeadingBid),
  E4 = N4.

test_get_next_itemid(Config) ->
  ItemIds = ?config(itemids, Config),
  [ItemId1, ItemId2] = ItemIds,
  {ItemId1, [ItemId2]} = auction:get_next_itemid(ItemIds),
  {ItemId2, []} = auction:get_next_itemid([ItemId2]),
  {undefined, undefined} = auction:get_next_itemid([]).