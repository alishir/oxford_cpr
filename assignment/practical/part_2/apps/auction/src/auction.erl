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

-type unknown_auction() :: term().

%%% Auction API ---------------------------------------------------------------
%% @doc 
-spec start_link(reference()) -> {ok, pid()} | {error, unknown_auction()}.
start_link(AuctionId) ->
  case auction_data:get_items(AuctionId) of
    {ok, ItemIds} ->
      % returns {ok, Pid} if successful
      gen_statem:start_link({local, ?MODULE}, 
                            ?MODULE, 
                            [AuctionId, ItemIds], 
                            []);
    {error, unknown_auction} ->
      {error, unknown_auction}
  end.

%% @doc
-spec
bid(AuctionId, ItemId, Bid, Bidder) ->
  ok.

% stop() ->
%   gen_server:call(?MODULE, stop).

%%% Gen StateM Callbacks ------------------------------------------------------
init([AuctionId, ItemIds]) ->
  State = auction_live,
  Data = #{auctionid => AuctionId, remainingitemids => ItemIds},
  {ok, State, Data}.

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