%%%----------------------------------------------------------------------------
%% File: bidder_client_server_sup_SUITE.erl
%% @author Nicholas Drake
%% @doc bidder_client_server_SUITE
%% @end
%%%----------------------------------------------------------------------------
-module(bidder_client_server_sup_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([test_start_link/1,
         test_stop/1]).

all() ->
  [test_start_link,
   test_stop].

%%% suite setup & tear down ---------------------------------------------------
init_per_suite(Config) ->
  ok = application:start(auction_server),
  Config.

end_per_suite(_Config) ->
  application:stop(auction_server),
  ok.

%% supervisor test -----------------------------------------------------------
test_start_link(_Config) ->
  BidderName1 = "elon musk",
  {ok, _} = bidder_client_server_sup:start_link(BidderName1),
  [] = bidder_client_server:get_auctions(BidderName1),
  ok = bidder_client_server:stop(BidderName1).

test_stop(_Config) ->
  BidderName2 = "jeff bezos",
  {ok, ChildPid} = bidder_client_server_sup:start_link(BidderName2),
  true = bidder_client_server_sup:stop(BidderName2).
