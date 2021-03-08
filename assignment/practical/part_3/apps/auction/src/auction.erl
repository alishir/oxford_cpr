%%%----------------------------------------------------------------------------
%% File: auction.erl
%% @author Nicholas Drake
%% @doc auction
%% @end
%%%----------------------------------------------------------------------------

-module(auction).

-behaviour(gen_statem).

-export([start_link/1, bid/4, subscribe/1]).
-export([auction_item/3, auction_ended/3]).
-export([init/1, callback_mode/0, terminate/3]).
-export([get_starting_bid/3, check_for_invalid_bid/7, check_leading_bid/6, 
  add_winning_bidder/4, get_next_itemid/1]).

-type itemid() :: {integer(), reference()}.
-type bidderid() :: {nonempty_string(), reference()}.

%%% Auction API ---------------------------------------------------------------
%% @doc Creates a gen_statem process that is linked to the calling process 
%% adding it to a supervision tree (assuming called by supervisor). It gets the
%% initial state by calling auction_data:get_items_and_lock_auction which both 
%% gets a list of ItemIds in a given AuctionId (in lexicographic order) but 
%% also updates the AuctionId to locked so no more items can be added. All
%% this is done in a nested transaction to prevent any race conditions 
%% occurring. 
-spec start_link(reference()) -> {ok, pid()} | {error, unknown_auction}.
start_link(AuctionId) ->
  case auction_data:get_items_and_lock_auction(AuctionId) of
    % auction exists but no items so no point in having the auction
    {ok, []} ->
      {error, unknown_auction};
    % at least one item
    {ok, [HeadItemId | TailItemIds]} ->
      pubsub:publish(AuctionId, {auction_event, auction_started}),
      % returns {ok, Pid} if successful
      gen_statem:start_link({local, ?MODULE}, 
                            ?MODULE, 
                            [AuctionId, HeadItemId, TailItemIds], 
                            []);
    {error, unknown_auction} ->
      {error, unknown_auction}
  end.

%% @doc API for users to bid on a current item ItemId in a given AuctionId.
%% They will include their Bid and their Bidder Id. The user will either be
%% informed they are leading the ItemId auction, or they are non-leading and
%% what the leading bid is, or that there is some error.
-spec bid(reference(), itemid(), non_neg_integer(), bidderid()) ->
{ok, leading | {not_leading, non_neg_integer()}} | 
{error, invalid_auction | invalid_item | auction_ended | item_already_sold}.
bid(AuctionId, ItemId, Bid, Bidder) ->
  gen_statem:call(?MODULE, {bid, AuctionId, ItemId, Bid, Bidder}).

%% @doc A way to connect the publish-subscribe engine to the auction engine
%% creating channels when auctions are created and generate events.
-spec subscribe(reference()) -> {ok, reference()} | {error, unknown_auction}.
subscribe(AuctionId) ->
  case pubsub:subscribe(AuctionId) of
    ok -> 
      {ok, ChannelPid} = pubsub:monitor(AuctionId),
      Reference = erlang:monitor(process, ChannelPid),
      {ok, Reference};
    {error, unknown_channel} -> 
      {error, unknown_auction}
  end.

%%% Gen StateM Callbacks ------------------------------------------------------
init([AuctionId, HeadItemId, TailItemIds]) ->
  {ok, {HeadItemId, Description, StartingBid}} = 
    auction_data:get_item(AuctionId, HeadItemId),
  pubsub:publish(
    AuctionId, 
    {auction_event, {new_item, HeadItemId, Description, StartingBid}}),
  State = auction_item,
  Data = #{auctionid => AuctionId, 
           current_itemid => HeadItemId,
           remaining_itemids => TailItemIds, 
           auctioned_itemids => [],
           starting_bid => undefined,
           leading_bid => undefined,
           leading_bidder => undefined},
  {ok, State, Data, [{state_timeout, 10000, next_item}]}.

callback_mode() ->
  state_functions.

%% gen_statem calls StateName(call, Event, Data) and returns either 
%% {next_state, NewStateName, NewData} or 
%% {next_state, NewStateName, NewData, Actions}
auction_item({call,From}, 
             {bid, BidAuctionId, BidItemId, Bid, Bidder},
             #{auctionid := AuctionId, 
               current_itemid := CurrentItemId,
               remaining_itemids := _RemainingItemIds,
               auctioned_itemids := AuctionedItemIds,
               starting_bid := StartingBid,
               leading_bid := LeadingBid,
               leading_bidder := _LeadingBidder} = Data) ->
  % check that bid is valid
  ErrorState = check_for_invalid_bid(Data, From, AuctionId, BidAuctionId, 
    CurrentItemId, BidItemId, AuctionedItemIds),
  if 
    ErrorState =/= undefined -> 
      ErrorState; % reply with {error, ...}
  % check if bid is leading
    true -> 
      % if new item need to get starting_bid
      NewStartingBid = get_starting_bid(AuctionId, CurrentItemId, StartingBid),
      check_leading_bid(Data#{starting_bid := StartingBid}, 
                        From, 
                        Bid, 
                        Bidder, 
                        NewStartingBid, 
                        LeadingBid)
  end;
%% gen_statem calls {state_timeout, Time, EventContent}
auction_item(state_timeout, 
             next_item,
             #{auctionid := AuctionId, 
               current_itemid := CurrentItemId,
               remaining_itemids := RemainingItemIds,
               auctioned_itemids := AuctionedItemIds,
               starting_bid := _StartingBid,
               leading_bid := LeadingBid,
               leading_bidder := LeadingBidder} = Data) ->
  % if there was a winner add them
  add_winning_bidder(AuctionId, CurrentItemId, LeadingBid, LeadingBidder),
  {NewCurrentItemId, NewRemainingItemIds} = get_next_itemid(RemainingItemIds),
  if 
    NewCurrentItemId =:= undefined ->
      pubsub:publish(AuctionId, {auction_event, auction_closed}),
      {next_state,
       auction_ended,
       Data};
    true -> 
      % new item so set 
      {ok, {NewCurrentItemId, NewDescription, NewStartingBid}} = 
        auction_data:get_item(AuctionId, NewCurrentItemId),
      pubsub:publish(
        AuctionId, 
        {auction_event, 
         {new_item, NewCurrentItemId, NewDescription, NewStartingBid}}),
      {next_state,
       auction_item, 
       Data#{% auctionid is the same
             current_itemid := NewCurrentItemId,
             remaining_itemids := NewRemainingItemIds,
             auctioned_itemids := [CurrentItemId | AuctionedItemIds],
             starting_bid := undefined,
             leading_bid := undefined,
             leading_bidder := undefined}, 
       [{state_timeout, 10000, next_item}]}
  end.

auction_ended({call,From}, 
              {bid, _, _, _, _},
              Data) ->
  {keep_state,
   Data,
   [{reply, From, {error, auction_ended}}]}.

%%% Helper functions ----------------------------------------------------------

%% if we have a new item then need to get starting bid, or if not new item use 
%% the bid we already have
get_starting_bid(AuctionId, CurrentItemId, StartingBid) ->
  if 
    StartingBid =:= undefined ->
      {ok, {_, _, GottenStartingBid}} = 
        auction_data:get_item(AuctionId, CurrentItemId),
      GottenStartingBid;
    true ->
      StartingBid
  end.

check_for_invalid_bid(Data, From, AuctionId, BidAuctionId, CurrentItemId, 
  BidItemId, AuctionedItemIds) ->
  % lists:member cannot be in a guard expression
  case lists:member(BidItemId, AuctionedItemIds) of
    true ->
      {keep_state, 
       Data, 
       [{reply, From, {error, item_already_sold}}]};
    false ->
      if 
        AuctionId =/= BidAuctionId -> 
          {keep_state, 
           Data, 
           [{reply, From, {error, invalid_auction}}]};
        CurrentItemId =/= BidItemId ->
          {keep_state, 
           Data, 
           [{reply, From, {error, invalid_item}}]};
        true ->
          undefined
      end
  end.

check_leading_bid(Data, From, Bid, Bidder, StartingBid, LeadingBid) ->
  if 
    LeadingBid =:= undefined ->   % first bid
      if
        Bid < StartingBid ->  % not_leading because lower than StartingBid 
          {keep_state, 
           Data, 
           [{reply, From, {ok, {not_leading, StartingBid}}}]};
        true -> % leading because higher than StartingBid and no LeadingBid
          AuctionId = maps:get(auctionid, Data),
          ItemId = maps:get(current_itemid, Data),
          pubsub:publish(AuctionId, 
                         {auction_event, {new_bid, ItemId, Bid}}),
          {next_state,
           auction_item, 
           Data#{leading_bid := Bid,
                 leading_bidder := Bidder}, 
           [{reply, From, {ok, leading}},
            {state_timeout, 10000, next_item}]}
      end;
    true -> % there is a LeadingBid already
      if
        Bid =< LeadingBid -> % not_leading because lower than LeadingBid
          {keep_state, 
           Data, 
           [{reply, From, {ok, {not_leading, LeadingBid}}}]};
        true -> % leading because higher than LeadingBid
          AuctionId = maps:get(auctionid, Data),
          ItemId = maps:get(current_itemid, Data),
          pubsub:publish(AuctionId, 
                         {auction_event, {new_bid, ItemId, Bid}}),
          {next_state,
           auction_item, 
           Data#{leading_bid := Bid,
                 leading_bidder := Bidder}, 
           [{reply, From, {ok, leading}},
            {state_timeout, 10000, next_item}]}
      end
  end.

%% if no winner then do not save 
add_winning_bidder(AuctionId, CurrentItemId, LeadingBid, LeadingBidder) ->
  if 
    LeadingBid =/= undefined -> % we have a winner!
      pubsub:publish(
        AuctionId, 
        {auction_event, {item_sold, CurrentItemId, LeadingBid}}),
      auction_data:add_winning_bidder(
        AuctionId, CurrentItemId, LeadingBid, LeadingBidder);
    true -> 
      ok
  end.

%% see if there are any more ItemIds to auction
get_next_itemid(RemainingItemIds) ->
  case RemainingItemIds of
    [] -> 
      {undefined, undefined};
    [NewCurrentItemId | NewRemainingItemIds] -> 
      {NewCurrentItemId, NewRemainingItemIds}
  end.

terminate(_Reason, _State, _Data) ->
  ok.