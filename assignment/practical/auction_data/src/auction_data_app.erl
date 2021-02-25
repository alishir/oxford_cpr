%%%----------------------------------------------------------------------------
%% File: auction_data_app.erl
%% @author Nicholas Drake
%% @doc auction_data public API
%% @end
%%%----------------------------------------------------------------------------

-module(auction_data_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    auction_data_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
