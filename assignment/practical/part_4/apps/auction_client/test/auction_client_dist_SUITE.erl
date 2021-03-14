-module(auction_client_dist_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, 
         groups/0, 
         init_per_group/2, 
         end_per_group/2]).
-export([test_auction_client/1]).

all() -> [{group, main}, 
          {group, backup}].

groups() -> [{main,
             [],
             [test_auction_client]},
            {backup,
             [],
             [test_auction_client]}].

%%% group setup & tear down ---------------------------------------------------
init_per_group(main, Config) ->
  application:start(auction_client),
  Config;
init_per_group(backup, Config) ->
  application:start(auction_client),
  Config.

end_per_group(main, _Config) ->
  application:stop(auction_client);
end_per_group(backup, _Config) ->
  application:stop(auction_client).

test_auction_client(_Config) ->
  ok.
  % auction_client:get_auctions("elon musk").