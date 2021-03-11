%%%----------------------------------------------------------------------------
%% File: auction_sup_SUITE.erl
%% @author Nicholas Drake
%% @doc Pubsub supervisor test suite
%% @end
%%%----------------------------------------------------------------------------
-module(pubsub_sup_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([test_start_link/1]).

all() ->
  [test_start_link].

%% supervisor test -----------------------------------------------------------
test_start_link(_Config) ->
  {ok, PubsubPid} = pubsub:start_link(),
  AuctionId = make_ref(),
  ok = pubsub:create_channel(AuctionId),
  {error, {already_started, PubsubPid}} = pubsub:start_link().