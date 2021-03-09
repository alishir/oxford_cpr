%%%----------------------------------------------------------------------------
%% File: auction_sup_SUITE.erl
%% @author Nicholas Drake
%% @doc Auction supervisor test suite
%% @end
%%%----------------------------------------------------------------------------
-module(auction_sup_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([test_start_link/1,
         test_start_auction/1,
         test_stop_auction/1]).

all() ->
  [test_start_link,
   test_start_auction,
   test_stop_auction].

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

%%% testcase setup & tear down ------------------------------------------------
init_per_testcase(test_start_auction, Config) ->
  {ok, _Pid} = pubsub:start_link(),
  {ok, SupervisorPid} = auction_sup:start_link(),
  % no items
  {ok, AuctionId1} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId1),
  % added items
  {ok, AuctionId2} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId2),
  StartingBid1 = 0,
  StartingBid2 = 1,
  AuctionItems2 = 
    [{"book", "fiction", StartingBid2}, {"hat", "blue cap", StartingBid1}],
  {ok, [{_, "hat"}, {_, "book"}]} = 
    auction_data:add_items(AuctionId2, AuctionItems2),
  % added items
  {ok, AuctionId3} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId3),
  StartingBid3 = 3,
  StartingBid4 = 2,
  AuctionItems3 = 
    [{"cup", "for coffee", StartingBid3}, {"computer", "apple", StartingBid4}],
  {ok, [{_, "computer"}, {_, "cup"}]} = 
    auction_data:add_items(AuctionId3, AuctionItems3),
  [{supervisor, SupervisorPid} | 
    [{auction, [AuctionId1, AuctionId2, AuctionId3]} | 
      Config]];
init_per_testcase(test_stop_auction, Config) ->
  {ok, _Pid} = pubsub:start_link(),
  {ok, SupervisorPid} = auction_sup:start_link(),
  % no items
  {ok, AuctionId} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId),
  StartingBid1 = 0,
  StartingBid2 = 1,
  AuctionItems = 
    [{"book", "fiction", StartingBid2}, {"hat", "blue cap", StartingBid1}],
  {ok, [{_, "hat"}, {_, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems),
  [{supervisor, SupervisorPid} | 
    [{auction, AuctionId} | 
      Config]];
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(test_start_auction, Config) ->
  [AuctionId1, AuctionId2, AuctionId3] = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId1),
  ok = auction_data:remove_auction(AuctionId2),
  ok = auction_data:remove_auction(AuctionId3),
  pubsub:stop();
end_per_testcase(test_stop_auction, Config) ->
  AuctionId = ?config(auction, Config),
  ok = auction_data:remove_auction(AuctionId),
  pubsub:stop();
end_per_testcase(_, _Config) ->
  ok.

%%% supervisor test -----------------------------------------------------------
test_start_link(_Config) ->
  {ok, SupervisorPid} = auction_sup:start_link(),
  % should be empty as have dynamic children
  [] = supervisor:which_children(SupervisorPid),
  {error, {already_started, SupervisorPid}} = auction_sup:start_link().

test_start_auction(Config) ->
  [AuctionId1, AuctionId2, AuctionId3] = ?config(auction, Config),
  SupervisorPid = ?config(supervisor, Config),
  InvalidAuctionId = make_ref(),
  % invalid auction id
  {error, unknown_auction} = auction_sup:start_auction(InvalidAuctionId),
  % no items
  {error, unknown_auction} = auction_sup:start_auction(AuctionId1),
  % can have more than two auctions if valid 
  {ok, AuctionPid1} = auction_sup:start_auction(AuctionId2),
  {ok, AuctionPid2} = auction_sup:start_auction(AuctionId3),
  [{undefined, AuctionPid1, worker, [auction]}, 
   {undefined, AuctionPid2, worker, [auction]}] =
    supervisor:which_children(SupervisorPid).

test_stop_auction(Config) ->
  AuctionId = ?config(auction, Config),
  SupervisorPid = ?config(supervisor, Config),
  {ok, AuctionPid} = auction_sup:start_auction(AuctionId),
  [{undefined, AuctionPid, worker, [auction]}] =  
    supervisor:which_children(SupervisorPid),
  ok = auction_sup:stop_auction(AuctionPid),
  [] = supervisor:which_children(SupervisorPid).