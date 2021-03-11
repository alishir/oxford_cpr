%%%-------------------------------------------------------------------
%% @doc auction_house_client public API
%% @end
%%%-------------------------------------------------------------------

-module(auction_house_client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    auction_house_client_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
