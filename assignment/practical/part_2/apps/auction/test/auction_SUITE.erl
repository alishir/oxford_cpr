-module(auction_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2, 
         end_per_testcase/2]).

