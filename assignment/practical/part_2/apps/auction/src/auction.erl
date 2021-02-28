%%%----------------------------------------------------------------------------
%% File: auction.erl
%% @author Nicholas Drake
%% @doc auction
%% @end
%%%----------------------------------------------------------------------------

-module(auction).

-behaviour(gen_statem).

-export([start_link/1, bid/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-type itemid() :: {integer(), reference()}.
-type bidderid() :: {reference()}.

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
      {error, unknown_auction}
    % at least one item
    {ok, [HeadItemId | TailItemIds]} ->
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
{ok, leading | {non_leading, non_neg_integer()}} | 
{error, invalid_auction | invalid_item | auction_ended | item_already_sold}.
bid(AuctionId, ItemId, Bid, Bidder) ->
  gen_statem:call(?MODULE, {bid, AuctionId, ItemId, Bid, Bidder}).

%%% Gen StateM Callbacks ------------------------------------------------------
init([AuctionId, HeadItemId, TailItemIds]) ->
  State = auction_item,
  Data = #{auction_id => AuctionId, 
           current_itemid => HeadItemId,
           remaining_itemids => TailItemIds, 
           auctioned_itemids => [],
           starting_bid => undefined,
           leading_bid => undefined,
           leading_bidder => undefined},
  {ok, State, Data}.

callback_mode() ->
  state_functions.

%% gen_statem calls StateName(call, Event, Data) and returns either 
%% {next_state, NewStateName, NewData} or 
%% {next_state, NewStateName, NewData, Actions}
auction_item({call,From}, 
             {bid, BidAuctionId, BidItemId, Bid, Bidder},
             #{auctionid := AuctionId, 
               current_itemid := CurrentItemId,
               remaining_itemids := RemainingItemIds,
               auctioned_itemids := AuctionedItemIds,
               starting_bid := StartingBid,
               leading_bid := LeadingBid,
               leading_bidder := LeadingBidder} = Data) ->
  % if new item need to get starting_bid
  NewStartingBid = get_starting_bid(AuctionId, CurrentItemId, StartingBid),
  % check that bid is valid
  ErrorState = check_for_invalid_bid(Data, NewStartingBid, From, 
    AuctionId, BidAuctionId, CurrentItemId, BidItemId, AuctionedItemIds),
  if 
    ErrorState =/= undefined -> 
      ErrorState % reply with {error, ...}
  % check if bid is leading
    true -> 
      check_leading_bid(Data, Bid, Bidder, StartingBid, LeadingBid, LeadingBidder)
  end.

%% gen_statem calls {state_timeout, Time, EventContent}
auction_item(state_timeout, 
             next_item,
             #{auctionid := AuctionId, 
               current_itemid := CurrentItemId,
               remaining_itemids := RemainingItemIds,
               auctioned_itemids := AuctionedItemIds,
               starting_bid := StartingBid,
               leading_bid := LeadingBid,
               leading_bidder := LeadingBidder} = Data) ->
  save_winning_bidder(AuctionId, CurrentItemId, LeadingBid, LeadingBidder),
  {NewCurrentItemId, NewRemainingItemIds} = get_next_itemid(RemainingItemIds),
  if 
    NewCurrentItemId =:= undefined ->
      % go to auction_ended
    true -> 
      % new item so set 
      {auction_item, 
       Data#{% auctionid is the same
             current_itemid := NewCurrentItemId,
             remaining_itemids := NewRemainingItemIds,
             auctioned_itemids := [CurrentItemId | AuctionedItemIds],
             starting_bid := undefined,
             leading_bid := undefined,
             leading_bidder := undefined}, 
       [{state_timeout, 10000, next_item}]}.


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

check_for_invalid_bid(Data, NewStartingBid, From, AuctionId, BidAuctionId, 
  CurrentItemId, BidItemId, AuctionedItemIds) ->
  if 
    AuctionId =/= BidAuctionId -> 
      {keep_state, 
       Data#{starting_bid := NewStartingBid}, 
       [{reply, From, {error, invalid_auction}}]};
    lists:member(BidItemId, AuctionedItemIds) ->
      {keep_state, 
       Data#{starting_bid := NewStartingBid}, 
       [{reply, From, {error, item_already_sold}}]};
    CurrentItemId =/= BidItemId ->
      {keep_state, 
       Data#{starting_bid := NewStartingBid}, 
       [{reply, From, {error, invalid_item}}]};
    % auction_ended - TODO
    true ->
      undefined
  end.

check_leading_bid(Data, Bid, Bidder, StartingBid, LeadingBid, LeadingBidder) ->
  if 
    LeadingBid =:= undefined ->   % first bid
      if
        Bid < StartingBid ->  % non_leading because lower than StartingBid 
          {keep_state, 
           Data#{starting_bid := NewStartingBid}, 
           [{reply, From, {ok, {non_leading, StartingBid}}}]};
        true -> % leading because higher than StartingBid and no LeadingBid
          {auction_item, 
           Data#{starting_bid := NewStartingBid,
                 leading_bid := Bid
                 leading_bidder := Bidder}, 
           [{reply, From, {ok, leading}},
            {state_timeout, 10000, next_item}]}
      end
    true -> % there is a LeadingBid already
      if
        Bid <= LeadingBid -> % non_leading because lower than LeadingBid
          {keep_state, 
           Data#{starting_bid := NewStartingBid}, 
           [{reply, From, {ok, {non_leading, LeadingBid}}}]};
        true -> % leading because higher than LeadingBid
          {auction_item, 
           Data#{starting_bid := NewStartingBid,
                 leading_bid := Bid
                 leading_bidder := Bidder}, 
           [{reply, From, {ok, leading}},
            {state_timeout, 10000, next_item}]}
      end

%% if no winner then do not save 
save_winning_bidder(AuctionId, CurrentItemId, LeadingBid, LeadingBidder) ->
  if 
    LeadingBid =/= undefined -> % we have a winner!
      auction_data:add_winning_bidder(
        AuctionId, CurrentItemId, LeadingBid, LeadingBidder).

%% see if there are any more ItemIds to auction
get_next_itemid(RemainingItemIds) ->
  case RemainingItemIds of
    [] -> 
      {undefined, undefined};
    [NewCurrentItemId | NewRemainingItemIds] -> 
      {NewCurrentItemId, NewRemainingItemIds}
      
% handle_call(_Event, _From, State) ->
%   {noreply, State}.

% handle_cast(_Event, State) ->
%   {noreply, State}.

% handle_info(_Event, State) ->
%   {noreply, State}.

% code_change(_OldVsn, State, _Extra) ->
%   {ok, State}.  

% terminate(_Reason, _State) ->
%   ok.