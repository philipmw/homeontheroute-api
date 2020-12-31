-module(earth).
-export([
  meters_between_coords/2,
  meters_between_stops/2
]).

-include_lib("eunit/include/eunit.hrl").
-include("earth.hrl").
-include("records/stop.hrl").
-include("test_data.hrl").

-spec deg_to_rad(number()) -> float().
deg_to_rad(D) ->
  D * math:pi() / 180.

deg_to_rad_test() ->
  ?assertEqual(math:pi()/2, deg_to_rad(90)).

hav(Theta) ->
  math:pow(math:sin(Theta / 2), 2).

meters_between_coords(CoordsA, CoordsB) ->
  % https://en.wikipedia.org/wiki/Haversine_formula#The_haversine_formula
  Phi1 = deg_to_rad(CoordsA#coords.lat),
  Phi2 = deg_to_rad(CoordsB#coords.lat),
  Lam1 = deg_to_rad(CoordsA#coords.lon),
  Lam2 = deg_to_rad(CoordsB#coords.lon),
  H = hav(Phi2 - Phi1) +
    math:cos(Phi1) * math:cos(Phi2) * hav(Lam2 - Lam1),
  2 * ?EARTH_RADIUS_METERS * math:asin( math:sqrt(H) ).

meters_between_stops(StopA, StopB) ->
  meters_between_coords(StopA#stop.coords, StopB#stop.coords).

meters_between_stops_test() ->
  ?assertMatch(
    Distance when Distance > 480 andalso Distance < 481,
    meters_between_stops(?TEST_STOP_B, ?TEST_STOP_C)).
