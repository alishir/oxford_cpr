%%%----------------------------------------------------------------------------
%% File: bidder_client.erl
%% @author Nicholas Drake
%% @doc Bidder Client server
%% @end
%%%----------------------------------------------------------------------------

-module(bidder_client_server).

-behaviour(gen_server).

-export([start_link/1, stop/1, get_auctions/1]). % bid/5, subscribe/1]).
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
  gen_server:cast({global, BidderName}, {get_auctions}).

% subscribe() ->
%   ok.

% unsubscribe() ->
%   ok.

% bid() ->
%   ok.


%%% Gen StateM Callbacks ------------------------------------------------------
init([Bidder]) ->
  State = #{bidder => Bidder,
            automated_bidding => #{}},
  {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Call, _From, State) ->
  {noreply, State}.

handle_cast({get_auctions}, State) ->
  ItemsList = auction_data:get_auctions(),
  ct:print("List of auctions: ~p~n", [ItemsList]),
  io:format("List of auctions: ~p~n", [ItemsList]),
  {noreply, State};
handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.