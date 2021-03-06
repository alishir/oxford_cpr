%%%----------------------------------------------------------------------------
%% File: auction_data.erl
%% @author Nicholas Drake
%% @doc auction data
%% @end
%%%----------------------------------------------------------------------------

-module(auction_data).
-behaviour(application).

-export([start/2, stop/1]).
-export([install/1]).
-export([
  create_auction/0, 
  add_items/2, 
  get_auctions/0, 
  get_items/1,
  get_item/2,
  remove_auction/1,
  remove_item/2
]).

% note in part_4 this definition is changed to include node() to work for
% distributed erlang as well
-type itemid() :: {integer(), reference()}.
-type item_info() :: {nonempty_string(), nonempty_string(), non_neg_integer()}.
-type itemid_info() :: {itemid(), nonempty_string(), non_neg_integer()}.

-record(auction_ids, {auction_id, 
                      locked}).
-record(auction_data, {item_id,
                       auction_id,
                       item, 
                       desc,
                       bid}).

%%% Setup ---------------------------------------------------------------------
start(normal, []) ->
  mnesia:wait_for_tables([auction_ids, 
                          auction_data], 5000).

stop(_) -> ok.  

install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(auction_ids,
                      [{type, set},
                       {attributes, record_info(fields, auction_ids)},
                       {disc_copies, Nodes}]),
  mnesia:create_table(auction_data,
                      [{attributes, record_info(fields, auction_data)},
                       {disc_copies, Nodes},
                       {type, ordered_set}]),
  rpc:multicall(Nodes, application, stop, [mnesia]).

%%% Auction Data API ----------------------------------------------------------

%% @doc Creates a new auction and all associated data structures for it.
-spec create_auction() -> {ok, reference()}.
create_auction() ->
  F = fun() ->
    AuctionId = make_ref(),
    mnesia:write(#auction_ids{auction_id=AuctionId, locked=false}),
    {ok, AuctionId}
  end,
  mnesia:activity(transaction, F).

%% @doc Adds items to a given auction.
-spec add_items(reference(), [item_info()]) -> 
  {ok, [{itemid(), nonempty_string()}]} | {error, unknown_auction}.
add_items(AuctionId, ItemsList) ->
  F = fun() ->
    AuctionInfo = mnesia:read({auction_ids, AuctionId}),
    case AuctionInfo of
      [] ->
        {error, unknown_auction};
      [{auction_ids, AuctionId, true}] -> 
        {error, unknown_auction};
      [_] ->
        ItemsIdList = lists:foldl(
          fun({Item, Desc, Bid}, Output) -> 
            ItemId = {erlang:monotonic_time(), make_ref()},
            mnesia:write(#auction_data{item_id=ItemId,
                                       auction_id=AuctionId,
                                       item=Item,
                                       desc=Desc,
                                       bid=Bid}),
            [{ItemId, Item}|Output] 
          end,
          [],
          ItemsList),
        {ok, ItemsIdList}
    end
  end,
  mnesia:activity(transaction, F).

%% @doc Gets a list of auctions
-spec get_auctions() -> {ok, [reference()]}.
get_auctions() ->
  F = fun() ->
    Auctions = mnesia:all_keys(auction_ids),
    {ok, Auctions}
  end,
  mnesia:activity(transaction, F).

%% @doc Gets a list of items for a specific auction. The list is in 
%% lexicographical order
-spec get_items(reference()) -> 
  {ok, [itemid()]} | {error, unknown_auction}.
get_items(AuctionId) ->
  F1 = fun(AuctionData, Output) ->
    case AuctionData of
      {auction_data, ItemId, AuctionId, _, _, _} -> 
        [ItemId | Output];
      _ ->
        Output
    end
  end,
  F2 = fun() ->
    case mnesia:read({auction_ids, AuctionId}) =:= [] of
      true ->
        {error, unknown_auction};
      false ->
        {ok, mnesia:foldr(F1, [], auction_data)}
    end
  end,
  mnesia:activity(transaction, F2).

%% @doc Gets item specific information 
-spec get_item(reference(), itemid()) -> 
  {ok, itemid_info()} | {error, unknown_item | unknown_auction}.
get_item(AuctionId, ItemId) ->
  F = fun() ->
    case mnesia:read({auction_ids, AuctionId}) =:= [] of
      true ->
        {error, unknown_auction};
      false ->
        case mnesia:read({auction_data, ItemId}) of 
          [{auction_data, ItemId, AuctionId, _, Desc, Bid}] ->
            {ok, {ItemId, Desc, Bid}};
          _ ->
            {error, unknown_item}
        end
    end
  end,
  mnesia:activity(transaction, F).

%% @doc Removes an auction
-spec remove_auction(reference()) -> ok | {error, unknown_auction}.
remove_auction(AuctionId) ->
  F = fun() ->
    case mnesia:read({auction_ids, AuctionId}) =:= [] of
      true ->
        {error, unknown_auction};
      false ->
        mnesia:delete({auction_ids, AuctionId})
    end
  end,
  mnesia:activity(transaction, F).

%% @doc Removes an item from an auction
-spec remove_item(reference(), itemid()) ->
  ok | {error, unknown_item, unknown_auction}.
remove_item(AuctionId, ItemId) ->
  F = fun() ->
    AuctionInfo = mnesia:read({auction_ids, AuctionId}),
    case AuctionInfo of
      [] ->
        {error, unknown_auction};
      [{auction_ids, AuctionId, true}] -> 
        {error, unknown_auction};
      [_] ->
        ItemInfo = mnesia:read({auction_data, ItemId}),
        case ItemInfo of
          [{auction_data, ItemId, AuctionId, _, _, _}] ->
            mnesia:delete({auction_data, ItemId}),
            ok;
          [] ->
            {error, unknown_item}
        end
    end
  end,
  mnesia:activity(transaction, F).