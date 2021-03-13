%%%----------------------------------------------------------------------------
%% File: bidder_client.erl
%% @author Nicholas Drake
%% @doc Bidder Client server
%% @end
%%%----------------------------------------------------------------------------

-module(bidder_client_server).

-behaviour(gen_server).

-export([start_link/1, stop/1, get_auctions/1, subscribe/2, unsubscribe/2]). 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, 
         terminate/2]).

-type itemid() :: {node(), integer(), reference()}.
-type item_info() :: {nonempty_string(), nonempty_string(), non_neg_integer()}.
-type itemid_info() :: {itemid(), nonempty_string(), non_neg_integer()}.
-type bidderid() :: {nonempty_string(), reference()}.

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
  gen_server:call({global, BidderName}, 
                  {bid, AuctionId, ItemId, Bid}).

%%% Gen StateM Callbacks ------------------------------------------------------
init([Bidder]) ->
  State = #{bidder => Bidder,
            auction_id_to_pid_map => #{},
            automated_bidding => #{}},
  {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({get_auctions}, _From, State) ->
  {ok, ItemsList} = auction_data:get_auctions(),
  io:format("List of auctions: ~p~n", [ItemsList]),
  {reply, ItemsList, State};
handle_call({subscribe, AuctionId}, _From, State) ->
  Result = auction:subscribe(AuctionId),
  case Result of
    {ok, MonitorPid} ->
      io:format("Subscribed to auction~n");
    {error, unknown_auction} ->
      io:format("Unknown auction~n")
  end,
  {reply, Result, State};
handle_call({unsubscribe, AuctionId}, _From, State) ->
  Result = pubsub:unsubscribe(AuctionId),
  case Result of
    ok ->
      io:format("Unsubscribed to auction~n");
    {error, unknown_auction} ->
      io:format("Unknown auction~n")
  end,
  {reply, Result, State};
handle_call({bid, AuctionId, ItemId, Bid}, _From, State) ->
  {noreply, State};
handle_call(_Call, _From, State) ->
  {noreply, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(
  {auction_event, {auction_started, AuctionId, AuctionPid}}, State) ->
  io:format("AuctionId ~p started~n", [AuctionId]),
  {noreply, State#{auction_id_to_pid_map := #{AuctionId => AuctionPid}}};
handle_info(Info, State) ->
  ct:print("~p", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.