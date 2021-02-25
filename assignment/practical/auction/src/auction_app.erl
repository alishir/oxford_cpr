%%%----------------------------------------------------------------------------
%% File: auction_app.erl
%% @author Nicholas Drake
%% @doc auction public API
%% @end
%%%----------------------------------------------------------------------------

-module(auction_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    auction_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
