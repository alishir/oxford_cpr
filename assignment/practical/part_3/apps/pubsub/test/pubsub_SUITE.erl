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

-export([test_start_link/1]).

all() ->
  [test_start_link].

%%% testcase setup & tear down ------------------------------------------------
init_per_testcase(test_start_link, Config) ->
  Config;
init_per_testcase(_, Config) ->
  {ok, _Pid} = pubsub:start_link(),
  Config.

end_per_testcase(_, _Config) ->
  pubsub:stop().

%%% pubsub tests --------------------------------------------------------------
test_start_link(_Config) ->
  {ok, _Pid} = pubsub:start_link().