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
         groups/0,
         init_per_group/2, 
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([test_start_link/1,
         test_create_channel/1,
         test_delete_channel/1,
         test_subscribe/1,
         test_unsubscribe/1,
         test_publish/1,
         test_monitor/1]).

-export([pub1/1,
         pub2/1,
         sub1/1,
         sub2/1]).

all() ->
  [test_start_link,
   test_create_channel,
   test_delete_channel,
   test_subscribe,
   test_unsubscribe,
   test_publish, 
   test_monitor, 
   {group, pubsub_session}].

groups() ->
  [{pubsub_session, 
    [], 
    [{group, pubs_and_subs}]},
   {pubs_and_subs, 
    [parallel], 
    [pub1, pub2, sub1, sub2]}].

%%% pubsub group setup and teardown -------------------------------------------
init_per_group(pubsub_session, Config) ->
  {ok, Pid} = pubsub:start_link(),
  unlink(Pid),
  Channel1 = make_ref(),
  Channel2 = make_ref(),
  Channel3 = make_ref(),
  [{channels, [Channel1, Channel2, Channel3]} | Config];
init_per_group(_, Config) ->
  Config.

end_per_group(pubsub_session, _Config) ->
  pubsub:stop();
end_per_group(_, _Config) ->
  ok.

%%% testcase setup & tear down ------------------------------------------------
init_per_testcase(test_start_link, Config) ->
  Config;
init_per_testcase(pub1, Config) ->
  Config;
init_per_testcase(pub2, Config) ->
  Config;
init_per_testcase(sub1, Config) ->
  Config;
init_per_testcase(sub2, Config) ->
  Config;
init_per_testcase(_, Config) ->
  {ok, _Pid} = pubsub:start_link(),
  Channel = make_ref(),
  [{channel, Channel} | Config].

end_per_testcase(pub1, _Config) ->
  ok;
end_per_testcase(pub2, _Config) ->
  ok;
end_per_testcase(sub1, _Config) ->
  ok;
end_per_testcase(sub2, _Config) ->
  ok;
end_per_testcase(_, _Config) ->
  pubsub:stop().

%%% pubsub unit tests ---------------------------------------------------------
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

%%% pubsub group parallel tests -----------------------------------------------
pub1(Config) ->
  [Channel1, Channel2, _] = ?config(channels, Config),
  % 0 s
  ok = pubsub:create_channel(Channel1),
  timer:sleep(2000),
  % 2 s
  ok = pubsub:publish(Channel1, {channel_event, {new_event, 5}}),
  ok = pubsub:create_channel(Channel2),
  timer:sleep(2000),
  % 4 s
  ok = pubsub:publish(Channel2, {channel_event, {new_vote, 7}}),
  timer:sleep(1000),
  % 5 s 
  ok = pubsub:publish(Channel1, {channel_event, {event_over}}),
  ok = pubsub:publish(Channel1, {channel_closed, {channel_1}}),
  ok = pubsub:publish(Channel2, {channel_closed, {channel_2}}).

pub2(Config) ->
  [Channel1, _, Channel3] = ?config(channels, Config),
  % 0 s
  timer:sleep(2000),
  % 2 s
  {error, duplicate_channel} = pubsub:create_channel(Channel1),
  ok = pubsub:create_channel(Channel3),
  timer:sleep(2000),
  % 4s
  ok = pubsub:publish(Channel3, {channel_event, {new_event, 3}}).

sub1(Config) ->
  [Channel1, Channel2, Channel3] = ?config(channels, Config),
  timer:sleep(1000),
  % 1 s
  ok = pubsub:subscribe(Channel1),
  {error, unknown_channel} = pubsub:subscribe(Channel2),
  receive
    Msg1 ->
      ct:print("sub1 ~p", [Msg1]),
      {channel_event, {new_event, 5}} = Msg1
  end,
  % 2 s
  timer:sleep(1000),
  % 3 s
  ok = pubsub:subscribe(Channel2),
  ok = pubsub:subscribe(Channel3),
  % here there is a race condition and we dont' want to recurse so repeat 
  % twice
  receive
    {channel_event, {new_vote, 7}} ->
      ct:print("sub1 ~p", [{channel_event, {new_vote, 7}}]);
    {channel_event, {new_event, 3}} ->
      ct:print("sub1 ~p", [{channel_event, {new_event, 3}}])
  end,
  receive
    {channel_event, {new_vote, 7}} ->
      ct:print("sub1 ~p", [{channel_event, {new_vote, 7}}]);
    {channel_event, {new_event, 3}} ->
      ct:print("sub1 ~p", [{channel_event, {new_event, 3}}])
  end,
  % 4 s
  ok = pubsub:unsubscribe(Channel1),
  % doesn't receive {event_over}
  receive
    Msg2 ->
      ct:print("sub1 ~p", [Msg2]),
      {channel_closed, {channel_2}} = Msg2
  end.
  % 5 s 

sub2(Config) ->
  [Channel1, _, _] = ?config(channels, Config),
  timer:sleep(1000),
  % 1 s
  ok = pubsub:subscribe(Channel1),
  receive
    Msg1 ->
      ct:print("sub2 ~p", [Msg1]),
      {channel_event, {new_event, 5}} = Msg1
  end,
  % 2 s
  % doesn't receive {channel_event, {new_vote, 7}
  receive
    Msg2 ->
      ct:print("sub2 ~p", [Msg2]),
      {channel_event, {event_over}} = Msg2
  end,
  % 5 s 
  receive
    Msg3 ->
      ct:print("sub2 ~p", [Msg3]),
      {channel_closed, {channel_1}} = Msg3
  end.
  % doesn't receive {channel_closed, {channel_2}}


