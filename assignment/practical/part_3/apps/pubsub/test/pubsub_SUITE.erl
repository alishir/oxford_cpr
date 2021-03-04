%%%----------------------------------------------------------------------------
%% File: pubsub_SUITE.erl
%% @author Nicholas Drake
%% @doc pubsub
%% @end
%%%----------------------------------------------------------------------------
-module(pubsub_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
         init_per_testcase/2,
         end_per_testcase/2]).

-export([test_start_link/1,
         test_create_channel/1,
         test_delete_channel/1,
         test_subscribe/1,
         test_unsubscribe/1,
         test_publish/1,
         test_monitor/1]).

all() ->
  [test_start_link,
   test_create_channel,
   test_delete_channel,
   test_subscribe,
   test_unsubscribe,
   test_publish, 
   test_monitor].

%%% testcase setup & tear down ------------------------------------------------
init_per_testcase(test_start_link, Config) ->
  Config;
init_per_testcase(_, Config) ->
  {ok, _Pid} = pubsub:start_link(),
  Channel = make_ref(),
  [{channel, Channel} | Config].

end_per_testcase(_, _Config) ->
  pubsub:stop().

%%% pubsub tests --------------------------------------------------------------
test_start_link(_Config) ->
  {ok, _Pid} = pubsub:start_link().

test_create_channel(Config) ->
  Channel = ?config(channel, Config),
  ok = pubsub:create_channel(Channel),
  {error, duplicate_channel} = pubsub:create_channel(Channel).

test_delete_channel(Config) ->
  Channel = ?config(channel, Config),
  {error, unknown_channel} = pubsub:delete_channel(Channel),
  ok = pubsub:create_channel(Channel),
  ok = pubsub:delete_channel(Channel).

test_subscribe(Config) ->
  Channel = ?config(channel, Config),
  {error, unknown_channel} = pubsub:subscribe(Channel),
  ok = pubsub:create_channel(Channel),
  ok = pubsub:subscribe(Channel).

test_unsubscribe(Config) ->
  Channel = ?config(channel, Config),
  {error, unknown_channel} = pubsub:unsubscribe(Channel),
  ok = pubsub:create_channel(Channel),
  ok = pubsub:unsubscribe(Channel).

test_publish(Config) ->
  Channel = ?config(channel, Config),
  Event = {pubsub_event, pubsub_started},
  {error, unknown_channel} = pubsub:publish(Channel, Event),
  ok = pubsub:create_channel(Channel),
  ok = pubsub:publish(Channel, Event).  

test_monitor(Config) ->
  Channel = ?config(channel, Config),
  {error, unknown_channel} = pubsub:monitor(Channel),
  ok = pubsub:create_channel(Channel),
  {ok, _} = pubsub:monitor(Channel).
  