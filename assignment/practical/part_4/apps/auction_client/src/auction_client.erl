%%%----------------------------------------------------------------------------
%% File: auction_client.erl
%% @author Nicholas Drake
%% @doc auction_client public API
%% @end
%%%----------------------------------------------------------------------------

-module(auction_client).

-behaviour(application).

-export([start/2, stop/1, get_auctions/1, subscribe/2, unsubscribe/2, 
bid/4, add_automated_bid_to_max/5]). 

start(normal, []) ->
    BidderName = "elon musk",
    auction_client_server_sup:start_link(BidderName);
% start({failover, Node}, Args) is only called when a start_phase key is 
% defined.
start({takeover, _OtherNode}, []) ->
    BidderName = "elon musk",
    auction_client_server_sup:start_link(BidderName).
  
stop(_State) ->
    ok.

get_auctions(BidderName) ->
    auction_client_server:get_auctions(BidderName).

subscribe(BidderName, AuctionId) ->
    auction_client_server:subscribe(BidderName, AuctionId).

unsubscribe(BidderName, AuctionId) ->
    auction_client_server:unsubscribe(BidderName, AuctionId).

bid(BidderName, AuctionId, ItemId, Bid) ->
    auction_client_server:bid(BidderName, AuctionId, ItemId, Bid).

add_automated_bid_to_max(BidderName, AuctionId, ItemId, StartBid, MaxBid) ->
    auction_client_server:add_automated_bid_to_max(BidderName, AuctionId, 
        ItemId, StartBid, MaxBid).