-module(person).
-export([
  direct_walk_mins/2
]).

-include("person.hrl").
-include("test_data.hrl").
-include_lib("eunit/include/eunit.hrl").

direct_walk_mins(StopA, StopB) ->
  Meters = earth:meters_between_stops(StopA, StopB),
  Meters / ?WALK_METERS_PER_MIN.

direct_walk_mins_test() ->
  ?assertMatch(
    Distance when Distance > 7 andalso Distance < 8,
      direct_walk_mins(?TEST_STOP_B, ?TEST_STOP_C)).

