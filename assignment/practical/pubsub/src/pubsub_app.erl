%%%-------------------------------------------------------------------
%% @doc pubsub public API
%% @end
%%%-------------------------------------------------------------------

-module(pubsub_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pubsub_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
