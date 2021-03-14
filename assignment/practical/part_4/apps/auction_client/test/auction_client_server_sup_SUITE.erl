%%%----------------------------------------------------------------------------
%% File: auction_client_server_sup_SUITE.erl
%% @author Nicholas Drake
%% @doc auction_client_server_SUITE
%% @end
%%%----------------------------------------------------------------------------
-module(auction_client_server_sup_SUITE).

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
  application:stop(mnesia),
  ok.

%% supervisor test -----------------------------------------------------------
test_start_link(_Config) ->
  BidderName1 = "elon musk",
  {ok, _} = auction_client_server_sup:start_link(BidderName1),
  [] = auction_client_server:get_auctions(BidderName1),
  ok = auction_client_server:stop(BidderName1).

test_stop(_Config) ->
  BidderName2 = "jeff bezos",
  {ok, _ChildPid} = auction_client_server_sup:start_link(BidderName2),
  true = auction_client_server_sup:stop(BidderName2).
