%%%----------------------------------------------------------------------------
%% File: auction_server.erl
%% @author Nicholas Drake
%% @doc Auction Server application
%% @end
%%%----------------------------------------------------------------------------

-module(auction_server).

-behaviour(application).

-export([start/2, 
         stop/1, 
         start_auction/1,
         stop_auction/1]).

%%% Application Callbacks -----------------------------------------------------
start(normal, []) ->
  auction_server_sup:start_link();
% start({failover, Node}, Args) is only called when a start_phase key is 
% defined.
start({takeover, _OtherNode}, []) ->
  auction_server_sup:start_link().

stop(_State) ->
  auction_server_sup:stop().

%%% Auction Server API --------------------------------------------------------
start_auction(AuctionId) ->
  auction_sup:start_auction(AuctionId).

stop_auction(AuctionPid) ->
  auction_sup:stop_auction(AuctionPid).