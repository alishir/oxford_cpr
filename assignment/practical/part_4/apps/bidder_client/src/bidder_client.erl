%%%-------------------------------------------------------------------
%% File: bidder_client.erl
%% @author Nicholas Drake
%% @doc bidder_client public API
%% @end
%%%-------------------------------------------------------------------

-module(bidder_client).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, [BidderName]) ->
    bidder_client_server_sup:start_link(BidderName);
% start({failover, Node}, Args) is only called when a start_phase key is 
% defined.
start({takeover, _OtherNode}, [BidderName]) ->
    bidder_client_server_sup:start_link(BidderName).
  
stop(_State) ->
    ok.