%%%----------------------------------------------------------------------------
%% File: pubsub.erl
%% @author Nicholas Drake
%% @doc Pubsub
%% @end
%%%----------------------------------------------------------------------------
-module(pubsub).

-behaviour(gen_server).

-export([start_link/0, create_channel/1, delete_channel/1, subscribe/1, 
         unsubscribe/1, publish/2]).
-export([stop/0, monitor/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Interface -----------------------------------------------------------------

%% @doc Starts the pubsub engine linking to its parent
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% @doc Creates channels where every channel is an auction
-spec create_channel(reference()) -> ok | {error, duplicate_channel}.
create_channel(Channel) ->
  gen_server:call({global, ?MODULE}, {create_channel, Channel}).  

%% @doc Deletes channels where every channel is an auction
-spec delete_channel(reference()) -> ok | {error, unknown_channel}.
delete_channel(Channel) ->
  gen_server:call({global, ?MODULE}, {delete_channel, Channel}).

%% @doc Users can subscribe to auctions only if the channel has been created
-spec subscribe(reference()) -> ok | {error, unknown_channel}.
subscribe(Channel) ->
  gen_server:call({global, ?MODULE}, {subscribe_channel, Channel}).

%% @doc When a channel is deleted all users are unsubscribed.
-spec unsubscribe(reference()) -> ok | {error, unknown_channel}.
unsubscribe(Channel) ->
  gen_server:call({global, ?MODULE}, {unsubscribe_channel, Channel}).

%% @doc A function to send notifications to the subscriber. Use the Pid of the 
%% process that called pubsub:subscribe/1 sending it Event as a message
-spec publish(reference(), term()) -> ok | {error, unknown_channel}.
publish(Channel, Event) ->
  gen_server:call({global, ?MODULE}, {publish_channel, Channel, Event}).

%%% Additional methods --------------------------------------------------------

-spec monitor(reference()) -> 
  {ok, reference()} | {error, unknown_channel}.
monitor(Channel) ->
  gen_server:call({global, ?MODULE}, {monitor_channel, Channel}).

-spec stop() -> ok.
stop() ->
    gen_server:call({global, ?MODULE}, stop).

%%% Gen Server callbacks ------------------------------------------------------
init([]) ->
  {ok, #{}}. % init with empty map

handle_call({create_channel, Channel}, _From, Channels) ->
  case maps:is_key(Channel, Channels) of
    true ->
      {reply, {error, duplicate_channel}, Channels};
    false ->
      {ok, ChannelPid} = gen_event:start_link(),
      UpdatedChannels = maps:put(Channel, ChannelPid, Channels),
      {reply, ok, UpdatedChannels}
  end;
handle_call({delete_channel, Channel}, _From, Channels) ->
  case maps:is_key(Channel, Channels) of
    true ->
      ChannelPid = maps:get(Channel, Channels),
      gen_event:stop(ChannelPid),
      UpdatedChannels = maps:remove(Channel, Channels),
      {reply, ok, UpdatedChannels};
    false ->
      {reply, {error, unknown_channel}, Channels}
  end;
handle_call({subscribe_channel, Channel}, From, Channels) ->
  case maps:is_key(Channel, Channels) of
    true ->
      ChannelPid = maps:get(Channel, Channels),
      {ClientPid, _} = From, 
      HandlerId = {channel_feed, ClientPid},
      gen_event:add_sup_handler(ChannelPid, HandlerId, [ClientPid]),
      {reply, ok, Channels};
    false ->
      {reply, {error, unknown_channel}, Channels}
  end;
handle_call({unsubscribe_channel, Channel}, From, Channels) ->
  case maps:is_key(Channel, Channels) of
    true ->
      ChannelPid = maps:get(Channel, Channels),
      {ClientPid, _} = From, 
      HandlerId = {channel_feed, ClientPid},
      gen_event:delete_handler(ChannelPid, HandlerId, [leave_feed]),
      {reply, ok, Channels};
    false ->
      {reply, {error, unknown_channel}, Channels}
  end;
handle_call({publish_channel, Channel, Event}, _From, Channels) ->
  case maps:is_key(Channel, Channels) of
    true ->
      ChannelPid = maps:get(Channel, Channels),
      Response = gen_event:notify(ChannelPid, Event),
      {reply, Response, Channels};
    false ->
      {reply, {error, unknown_channel}, Channels}
  end;
handle_call({monitor_channel, Channel}, _From, Channels) ->
  case maps:is_key(Channel, Channels) of
    true ->
      ChannelPid = maps:get(Channel, Channels),
      {reply, {ok, ChannelPid}, Channels};
    false ->
      {reply, {error, unknown_channel}, Channels}
  end;
handle_call(stop, _From, Channels) ->
  {stop, normal, ok, Channels};
handle_call(_Message, _From, Channels) ->
  {noreply, Channels}.

handle_cast(_Message, Channels) ->
  {noreply, Channels}.

handle_info(_Message, Channels) ->
  {noreply, Channels}.

terminate(_Reason, _Channels) ->
  ok.

code_change(_OldVsn, Channels, _Extra) ->
  {ok, Channels}.  
