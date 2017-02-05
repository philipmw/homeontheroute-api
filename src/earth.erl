-module(earth).

-export([
  meters_between_stops/2,
  direct_walk_mins/2
]).

-include_lib("eunit/include/eunit.hrl").
-include("test_data.hrl").
-include("earth.hrl").
-include("records/stop.hrl").

-spec deg_to_rad(X) -> float() when
  X :: float().
deg_to_rad(D) ->
  D * math:pi() / 180.

deg_to_rad_test() ->
  ?assertEqual(math:pi()/2, deg_to_rad(90)).

hav(Theta) ->
  math:pow(math:sin(Theta / 2), 2).

meters_between_stops(StopA, StopB) ->
  % https://en.wikipedia.org/wiki/Haversine_formula#The_haversine_formula
  Phi1 = deg_to_rad(StopA#stop.lat),
  Phi2 = deg_to_rad(StopB#stop.lat),
  Lam1 = deg_to_rad(StopA#stop.lon),
  Lam2 = deg_to_rad(StopB#stop.lon),
  H = hav(Phi2 - Phi1) +
    math:cos(Phi1) * math:cos(Phi2) * hav(Lam2 - Lam1),
  2 * ?EARTH_RADIUS_METERS * math:asin( math:sqrt(H) ).

is_within(V, T, E) ->
  (V >= (T-E)) and (V =< (T+E)).

meters_between_stops_test() ->
  ?assert(is_within(meters_between_stops(?TEST_STOP_A, ?TEST_STOP_B), 9685, 1)).

direct_walk_mins(StopA, StopB) ->
  Meters = meters_between_stops(StopA, StopB),
  Meters / ?WALK_METERS_PER_MIN.

direct_walk_mins_test() ->
  ?assert(is_within(direct_walk_mins(?TEST_STOP_A, ?TEST_STOP_B), 115, 1)).

