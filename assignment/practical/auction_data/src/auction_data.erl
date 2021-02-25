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

-type itemid() :: {integer(), reference()}.
-type item_info() :: {nonempty_string(), nonempty_string(), non_neg_integer()}.
-type itemid_info() :: {itemid(), nonempty_string(), non_neg_integer()}.
-type unknown_item() :: any().
-type unknown_auction() :: any().

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
  % auction_data_sup:start_link().

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

%%% API -----------------------------------------------------------------------

%% @doc Creates a new auction and all associated data structures for it.
-spec create_auction() -> {ok, reference()}.
create_auction() ->
  AuctionId = make_ref(),
  F = fun() ->
    mnesia:write(#auction_ids{auction_id=AuctionId, locked=false})
  end,
  mnesia:activity(transaction, F),
  {ok, AuctionId}.

%% @doc Adds items to a given auction.
-spec add_items(reference(), [item_info()]) -> 
  {ok, [{itemid(), nonempty_string()}]} | {error, unknown_auction()}.
add_items(AuctionId, ItemsList) ->
  
  ok.
  % ItemsIdList = lists:map(
  %   fun({Item, Desc, Bid}) -> 
  %     {{erlang:monotonic_time(), make_ref()}, Item, Desc, Bid} end,
  %   ItemsList),
  % try ets:insert(AuctionId, ItemsIdList) of
  %   true -> {ok, lists:map(fun({ItemId, Item, _, _}) -> {ItemId, Item} end, ItemsIdList)}
  % catch
  %   error:badarg -> {error, unknown_auction}
  % end.

%% @doc Gets a list of auctions
-spec get_auctions() -> {ok, [reference()]}.
get_auctions() ->
  ok.

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