%%%----------------------------------------------------------------------------
%% File: auction_data.erl
%% @author Nicholas Drake
%% @doc auction data
%% @end
%%%----------------------------------------------------------------------------

-module(auction_data).

-export([
  create_auction/0, 
  add_items/2, 
  get_auctions/0, 
  get_items/1,
  get_item/2,
  remove_auction/1,
  remove_item/2
]).

-type itemid() :: {integer(), reference()}.
-type item_info() :: {nonempty_string(), nonempty_string(), non_neg_integer()}.
-type itemid_info() :: {itemid(), nonempty_string(), non_neg_integer()}.
-type unknown_item() :: any().
-type unknown_auction() :: any().

%% @doc Creates a new auction and all associated data structures for it.
-spec create_auction() -> {ok, reference()}.
create_auction() ->
  % auction_table_name is unused
  Tid = ets:new(auction_table_name, [ordered_set, protected]),
  {ok, Tid}.

%% @doc Adds items to a given auction.
-spec add_items(reference(), [item_info()]) -> 
  {ok, [{itemid(), nonempty_string()}]} | {error, unknown_auction()}.
add_items(AuctionId, ItemsList) ->
  ItemsIdList = lists:map(
    fun({Item, Desc, Bid}) -> 
      {{erlang:monotonic_time(), make_ref()}, Item, Desc, Bid} end,
    ItemsList),
  try ets:insert(AuctionId, ItemsIdList) of
    true -> {ok, lists:map(fun({ItemId, Item, _, _}) -> {ItemId, Item} end, ItemsIdList)}
  catch
    error:badarg -> {error, unknown_auction}
  end.

%% @doc Gets a list of auctions
-spec get_auctions() -> {ok, [reference()]}.
get_auctions() ->
  {ok, ets:all()}.

%% @doc Gets a list of items for a specific auction. The list is in 
%% lexicographical order
-spec get_items(reference()) -> 
  {ok, [itemid()]} | {error, unknown_auction()}.
get_items(AuctionId) ->
  ok.

%% @doc Gets item specific information 
-spec get_item(reference(), itemid()) -> 
  {ok, itemid_info()} | {error, unknown_item() | unknown_auction()}.
get_item(AuctionId, ItemId) ->
  ok.

%% @doc Removes an auction
-spec remove_auction(reference()) -> ok | {error, unknown_auction()}.
remove_auction(AuctionId) ->
  ok.

%% @doc Removes an item from an auction
-spec remove_item(reference(), itemid()) ->
  ok | {error, unknown_item(), unknown_item()}.
remove_item(AuctionId, Item) ->
  ok.