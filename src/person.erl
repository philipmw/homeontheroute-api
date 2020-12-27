-module(person).
-export([
  direct_walk_secs/2
]).

-include("person.hrl").
-include("test_data.hrl").
-include_lib("eunit/include/eunit.hrl").

direct_walk_secs(StopA, StopB) ->
  Meters = earth:meters_between_stops(StopA, StopB),
  Meters / ?WALK_METERS_PER_SEC.

direct_walk_secs_test() ->
  ?assertMatch(
    Secs when Secs > 60*7 andalso Secs < 60*8,
      direct_walk_secs(?TEST_STOP_B, ?TEST_STOP_C)).

