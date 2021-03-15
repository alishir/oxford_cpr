-module(auction_simulation_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, 
         groups/0, 
         init_per_group/2, 
         end_per_group/2]).
-export([test_server/1,
         test_client/1]).

all() -> [{group, auction_server},
          {group, client}].

groups() -> [{auction_server, [], [test_server]},
             {client, [], [test_client]}].

%%% group setup & tear down ---------------------------------------------------
init_per_group(auction_server, Config) ->
  application:start(auction_server),
  Config;
init_per_group(client, Config) ->
  application:start(auction_client),
  Config.

end_per_group(auction_server, _Config) ->
  application:stop(auction_server);  
end_per_group(client, _Config) ->
  application:stop(auction_client).

test_server(_Config) ->
  {ok, AuctionId} = auction_data:create_auction(),
  ok = pubsub:create_channel(AuctionId),
  AuctionItems = 
    [{"book", "fiction", 1}, {"hat", "blue cap", 0}],
  {ok, [{_, "hat"}, {ItemId1, "book"}]} = 
    auction_data:add_items(AuctionId, AuctionItems).

test_client(_Config) ->
  BidderName = "elon musk",
  AuctionId = auction_client_server:get_auctions(BidderName),
  {ok, _} = auction_client_server:subscribe(BidderName, AuctionId).
