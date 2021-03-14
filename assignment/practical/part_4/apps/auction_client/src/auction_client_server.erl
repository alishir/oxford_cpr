%%%----------------------------------------------------------------------------
%% File: auction_client.erl
%% @author Nicholas Drake
%% @doc Bidder Client server
%% @end
%%%----------------------------------------------------------------------------

-module(auction_client_server).

-behaviour(gen_server).

-export([start_link/1, stop/1, get_auctions/1, subscribe/2, unsubscribe/2, 
         bid/4, add_automated_bid_to_max/5]). 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, 
         terminate/2]).

-type itemid() :: {node(), integer(), reference()}.

%%% Client API ----------------------------------------------------------------
-spec start_link(nonempty_string()) -> {ok, pid()}.
start_link(BidderName) ->
  Bidder = {BidderName, make_ref()},
  gen_server:start_link({global, BidderName},
                        ?MODULE,
                        [Bidder],
                        []).

-spec stop(nonempty_string()) -> ok.
stop(BidderName) ->
  gen_server:call({global, BidderName}, stop).

-spec get_auctions(nonempty_string()) -> {ok, [reference()]}.
get_auctions(BidderName) ->
  gen_server:call({global, BidderName}, {get_auctions}).

-spec subscribe(nonempty_string(), reference()) -> 
  {ok, reference()} | {error, unknown_auction}.
subscribe(BidderName, AuctionId) ->
  gen_server:call({global, BidderName}, {subscribe, AuctionId}).

-spec unsubscribe(nonempty_string(), reference()) -> 
  ok | {error, unknown_auction}.
unsubscribe(BidderName, AuctionId) ->
  gen_server:call({global, BidderName}, {unsubscribe, AuctionId}).

-spec bid(nonempty_string(), reference(), itemid(), non_neg_integer()) ->
  {ok, leading | {not_leading, non_neg_integer()}} | 
  {error, invalid_auction | invalid_item | auction_ended | item_already_sold}.
bid(BidderName, AuctionId, ItemId, Bid) ->
  gen_server:call({global, BidderName}, {bid, AuctionId, ItemId, Bid}).

-spec add_automated_bid_to_max(nonempty_string(), reference(), itemid(), 
  non_neg_integer(), non_neg_integer()) ->
  {ok, leading | {not_leading, non_neg_integer()}} | 
  {error, invalid_auction | invalid_item | auction_ended | item_already_sold}.
add_automated_bid_to_max(BidderName, AuctionId, ItemId, StartBid, MaxBid) ->
  gen_server:call({global, BidderName}, 
                  {add_automated_bid_to_max, AuctionId, ItemId, StartBid, 
                   MaxBid}),
  gen_server:call({global, BidderName}, 
                   {bid, AuctionId, ItemId, StartBid}).

%%% Private functions ---------------------------------------------------------
automated_bid(BidderName, AuctionId, ItemId, Bid, From) ->
  spawn(fun() ->
    BidResponse = 
      gen_server:call({global, BidderName}, {bid, AuctionId, ItemId, Bid}),
    gen_server:reply(From, BidResponse)
  end).

bid_automatically(State, AuctionId, ItemId, Bid) ->
  AutomatedBiddingMap = maps:get(automated_bidding, State),
  case maps:get(ItemId, AutomatedBiddingMap, undefined) of
    undefined ->
      {noreply, State}; % do nothing as no automated bid for this item
    {_From, Bid, _MaxBid} -> % last bid is our bid (tho race conditions)
      {noreply, State};
    {From, _LastBid, MaxBid} ->
      if 
        Bid + 1 =< MaxBid ->
          {BidderName, _} = maps:get(bidder, State),
          automated_bid(BidderName, AuctionId, ItemId, Bid + 1, From),
          UpdatedAutomatedBiddingMap = 
            AutomatedBiddingMap#{ItemId := {From, Bid + 1, MaxBid}},
          {noreply, State#{automated_bidding := UpdatedAutomatedBiddingMap}};
        true ->
          {noreply, State} % do nothing as too high
      end
  end.

%%% Gen StateM Callbacks ------------------------------------------------------
init([Bidder]) ->
  State = #{bidder => Bidder,
            auction_id_to_pid_map => #{},
            automated_bidding => #{}},
  {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({get_auctions}, _From, State) ->
  {ok, AuctionsList} = auction_data:get_auctions(),
  io:format("List of auctions: ~p~n", [AuctionsList]),
  {reply, AuctionsList, State};
handle_call({subscribe, AuctionId}, _From, State) ->
  Result = auction:subscribe(AuctionId),
  case Result of
    {ok, _MonitorPid} ->
      io:format("AuctionId ~p: Subscribed~n", [AuctionId]);
    {error, unknown_auction} ->
      io:format("AuctionId ~p: Unknown auction~n", [AuctionId])
  end,
  {reply, Result, State};
handle_call({unsubscribe, AuctionId}, _From, State) ->
  Result = pubsub:unsubscribe(AuctionId),
  case Result of
    ok ->
      io:format("AuctionId ~p: Unsubscribed~n", [AuctionId]);
    {error, unknown_auction} ->
      io:format("AuctionId ~p: Unknown auction~n", [AuctionId])
  end,
  {reply, Result, State};
handle_call({bid, AuctionId, ItemId, Bid}, _From, State) ->
  AuctionIdPidMap = maps:get(auction_id_to_pid_map, State),
  case maps:get(AuctionId, AuctionIdPidMap, undefined) of
    undefined ->
      io:format("AuctionId ~p: Auction not yet started~n", [AuctionId]),
      Result = {error, invalid_auction};
    AuctionPid ->
      Bidder = maps:get(bidder, State),
      io:format("AuctionId ~p: Submitted bid ~p~n", [AuctionId, Bid]),
      Result = auction:bid(AuctionPid, AuctionId, ItemId, Bid, Bidder)
  end,
  {reply, Result, State};
handle_call({add_automated_bid_to_max, AuctionId, ItemId, StartBid, MaxBid}, 
  From, State) ->
  AutomatedBiddingMap = maps:get(automated_bidding, State),
  UpdatedAutomatedBiddingMap = 
    maps:put(ItemId, {From, StartBid, MaxBid}, AutomatedBiddingMap),
  io:format("AuctionId ~p: Added automatic bid.~n", [AuctionId]),
  io:format("              Start bid: ~p, Max bid: ~p~n", [StartBid, MaxBid]),
  {reply, ok, State#{automated_bidding := UpdatedAutomatedBiddingMap}};
handle_call(_Call, _From, State) ->
  {noreply, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(
  {{AuctionId, auction_event}, {auction_started, AuctionPid}}, State) ->
  io:format("AuctionId ~p: Started~n", [AuctionId]),
  AuctionIdPidMap = maps:get(auction_id_to_pid_map, State),
  UpdatedAuctionIdPidMap = maps:put(AuctionId, AuctionPid, AuctionIdPidMap),
  {noreply, State#{auction_id_to_pid_map := UpdatedAuctionIdPidMap}};
handle_info({{AuctionId, auction_event}, 
  {new_item, ItemId, Description, Bid}}, State) ->
  io:format("AuctionId ~p: New item ~s with starting bid ~p~n", 
    [AuctionId, Description, Bid]),
  % need to check if there are any automated bids to be triggered
  bid_automatically(State, AuctionId, ItemId, Bid);
handle_info({{AuctionId, auction_event}, {new_bid, ItemId, Bid}}, State) ->
  io:format("AuctionId ~p: Bid ~p~n", [AuctionId, Bid]),
  % need to check if there are any automated bids to be triggered
  bid_automatically(State, AuctionId, ItemId, Bid);
handle_info({{AuctionId, auction_event}, 
  {item_sold, _ItemId, WinningBid}}, State) ->
  io:format("AuctionId ~p: Item sold. Winning bid ~p~n", 
    [AuctionId, WinningBid]),
  {noreply, State};
handle_info({{AuctionId, auction_event}, auction_closed}, State) ->
  io:format("AuctionId ~p: Closed~n", [AuctionId]),
  {noreply, State};
handle_info(Info, State) ->
  % shouldn't get here
  ct:print("Error: received  ~p", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.