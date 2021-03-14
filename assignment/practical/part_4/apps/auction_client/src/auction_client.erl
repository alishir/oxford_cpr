%%%-------------------------------------------------------------------
%% File: auction_client.erl
%% @author Nicholas Drake
%% @doc auction_client public API
%% @end
%%%-------------------------------------------------------------------

-module(auction_client).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, [BidderName]) ->
    auction_client_server_sup:start_link(BidderName);
% start({failover, Node}, Args) is only called when a start_phase key is 
% defined.
start({takeover, _OtherNode}, [BidderName]) ->
    auction_client_server_sup:start_link(BidderName).
  
stop(_State) ->
    ok.