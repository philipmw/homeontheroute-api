-module(trip).
-export([total_walk_mins/1]).

-include("records/sconn.hrl").
-include("records/stop.hrl").
-include("person.hrl").
-include("test_data.hrl").
-include_lib("eunit/include/eunit.hrl").

min_by(List, Fun) ->
  [Head|_Rest] = List,
  lists:foldl(
    fun(X, MinElem) -> min_by(MinElem, X, Fun) end,
    Head,
    List).

min_by(X, Y, Fun) ->
  case Fun(X) =< Fun(Y) of
    true -> X;
    _ -> Y
  end.

min_by_test() ->
  MinElem = min_by([3, 2, 1, 2, 3], fun(X) -> X*2 end),
  ?assertEqual(1, MinElem).

% At stop S, the cost of switching from transit mode A to B, expressed in Wait minutes.
mins_of_mode_switch(_S, _ModeA, _ModeA, _W) -> 0; % no switch
mins_of_mode_switch(_S, _ModeA, walk, _W) -> 0; % get off the bus and walk
mins_of_mode_switch(_S, walk, _ModeB, W) -> W; % get on the bus X
mins_of_mode_switch(_S, _ModeA, _ModeB, W) -> W. % transfer to bus X

mins_of_mode_switch_test() ->
  ?assertEqual(
    3,
    mins_of_mode_switch(stopC, routeBC, routeCD, 3)
  ).

total_walk_mins(Trip) -> total_walk_mins(Trip, 0).

total_walk_mins([], Mins) -> Mins;
total_walk_mins([{_WaitMins, walk, WalkMins, _ToStop}|Rest], WalkedMinsSoFar) ->
  total_walk_mins(Rest, WalkedMinsSoFar+WalkMins);
total_walk_mins([{_WaitMins, _TransitMode, _TravelMins, _ToStop}|Rest], WalkedMinsSoFar) ->
  total_walk_mins(Rest, WalkedMinsSoFar).

total_walk_mins_test() ->
  ?assertMatch(8, total_walk_mins([
    {0, walk, 5, stopA},
    {4, someBus, 10, stopB},
    {0, walk, 3, stopC}
  ])).

connections_from_stop(SConnsTab, StopAId, StopIdsToExclude) ->
  AllSConns = ets:lookup(SConnsTab, StopAId),
  lists:filter(
    fun (SConn) -> not lists:member(SConn#sconn.to_stop_id, StopIdsToExclude) end,
    AllSConns).

trip_test_connections_from_stop_1(Tabs) ->
  {sconns, SConnsTab} = lists:nth(1, ets:lookup(Tabs, sconns)),
  ?assertEqual(
    [?TEST_SCONN_YELLOW_C_D, ?TEST_SCONN_GREEN_C_F],
    connections_from_stop(SConnsTab, stopC, [])
  ).

trip_test_connections_from_stop_2(Tabs) ->
  {sconns, SConnsTab} = lists:nth(1, ets:lookup(Tabs, sconns)),
  ?assertEqual(
    [?TEST_SCONN_GREEN_C_F],
    connections_from_stop(SConnsTab, stopC, [stopD])
  ).

% Returns a list in the format:
%   [ {StopAId, MetersDistance}, {StopBId, MetersDistance}, ... ]
stops_walkable_from_coords(StopsTab, FromCoords, MaxWalkMinsAllowed) ->
  Distances = transit_data:ets_map(StopsTab, fun(StopX) ->
    {StopX, earth:meters_between_coords(FromCoords, StopX#stop.coords)} end),
  lists:filter(
    fun({StopX, MetersAway}) ->
      (MetersAway < MaxWalkMinsAllowed*?WALK_METERS_PER_MIN)
    end,
    Distances).

trip_test_stops_walkable_from_coords(Tabs) ->
  {stops, StopsTab} = lists:nth(1, ets:lookup(Tabs, stops)),
  ?assertMatch(
    [
      % These are out of order in real data, and I don't care enough
      % to make the test agnostic of ordering.
      {?TEST_STOP_B, DistanceB},
      {?TEST_STOP_A, DistanceA}
    ] when DistanceA > 111 andalso DistanceA < 112
      andalso DistanceB > 778 andalso DistanceB < 779,
      stops_walkable_from_coords(StopsTab, #coords{lat=47, lon=-122}, ?MAX_WALK_MINS)
  ).

% All stops surrounding the input stop, excluding the given set of stops.
% Returns a list in the format:
%   [ {StopAId, MetersDistance}, {StopBId, MetersDistance}, ... ]
stops_walkable_from_stop(StopsTab, FromStopId, StopIdsExcluded, MaxWalkMinsAllowed) ->
  [FromStop] = transit_data:stop(StopsTab, [FromStopId]),
  lists:filter(
    fun({StopX, _}) ->
      (not lists:member(StopX#stop.id, StopIdsExcluded))
    end,
    stops_walkable_from_coords(StopsTab, FromStop#stop.coords, MaxWalkMinsAllowed)).

trip_test_stops_walkable_from_stop(Tabs) ->
  {stops, StopsTab} = lists:nth(1, ets:lookup(Tabs, stops)),
  ?assertMatch(
    [{?TEST_STOP_B, Distance}] when Distance > 667 andalso Distance < 668,
      stops_walkable_from_stop(StopsTab, stopA, [stopA], ?MAX_WALK_MINS)
  ).

-spec total_mins([{number(), _, number(), _}]) -> number().
total_mins(Routes) -> lists:foldl(
  fun ({ModeSwitchMins, _TM, TransitMins, _StopB}, AccIn) -> ModeSwitchMins + TransitMins + AccIn end,
  0,
  Routes
).

total_mins_test() ->
  Mins = total_mins([
    {0, walk, 5, stopA},
    {5, route5, 3, stopB}
  ]),
  ?assertEqual(13, Mins).

dbgindent(StopsVisited) ->
  string:chars($., 3*length(StopsVisited)).

-record(tripstate, {
  stopsVisited
}).
-define(FRESH_TRIP_STATE, #tripstate{stopsVisited = []}).

% Find optimal trip between Stop A and Stop Z, when you arrived at Stop A
% using a specific mode of transit.
% Transit Modes: walk | RouteId
%
% Base case:
%   We arrived at Stop Z.  (Stop A = StopZ)
% Inductive cases:
%   a) Find a <<stops connection>> from Stop A to Stop B, then
%      solve the sub-problem of Optimal Route from B to Z.
%   b) Walk from Stop A to some Stop B within walking distance of A,
%      then solve the sub-problem of Optimal Route from B to Z.
%
% Supposing input: TransitModeToA=Route 40, StopA=40040, StopZ=40044
% Output: a list of transit segments
% [
%   {<mode-switch-mins>, <transit-mode>, <transit-time>, <to-stop-X>},
%   ...
%   {<mode-switch-mins>, <transit-mode>, <transit-time>, <to-stop-40044>}
% ]
% Example output:
% [
%   {3 min, Route 40, 2 min, Stop40041}
%   {0 min, Route 40, 2 min, Stop40042}
%   {0 min, Route 40, 4 min, Stop40043}
%   {5 min, Route 28, 3 min, Stop40044}
% ]
optimal_trip(Tabs, TransitModeToA, WalkedMins, StopAId, StopBId) ->
  optimal_trip(Tabs, TransitModeToA, WalkedMins, StopAId, StopBId, ?FRESH_TRIP_STATE).

optimal_trip(_Tabs, _TransitModeToA, _WalkedMins, StopId, StopId, _State) -> [];
optimal_trip(Tabs, TransitModeToA, WalkedMins, StopAId, StopZId, State) ->
  io:fwrite("~soptimal_trip(~w, ~w, ~w, ~w, ~w) invoked...~n", [dbgindent(State#tripstate.stopsVisited), State, TransitModeToA, WalkedMins, StopAId, StopZId]),
  {stops, StopsTab} = lists:nth(1, ets:lookup(Tabs, stops)),
  {sconns, SConnsTab} = lists:nth(1, ets:lookup(Tabs, sconns)),

  StopsVisited = State#tripstate.stopsVisited,
  MaxWalkMinsAllowed = ?MAX_WALK_MINS - WalkedMins,

  % We are assembling this list:
  % [
  %   [{<mode-switch-mins>, <transit-mode>, <transit-time>, <to-stop-X>}, [...optimal-route-from-X]]
  %   [{3 min, Route 40, 2 min, Stop40040}, [...optimal route from Stop40040]],
  %   [{5 min, Route 28, 3 min, Stop28028}, [...optimal route from Stop28028]],
  % ]

  TripsRidingThruStopB = [
    [{mins_of_mode_switch(StopAId, TransitModeToA, SConn#sconn.transit_mode, SConn#sconn.wait_mins),
      SConn#sconn.transit_mode,
      SConn#sconn.travel_mins,
      SConn#sconn.to_stop_id} |
      optimal_trip(Tabs,
                   SConn#sconn.transit_mode,
                   WalkedMins,
                   SConn#sconn.to_stop_id,
                   StopZId,
                   State#tripstate{stopsVisited=[StopAId]++StopsVisited})] ||
    SConn <- connections_from_stop(SConnsTab, StopAId, [StopAId]++StopsVisited)
  ],
  io:fwrite("~sTripsRidingThruStopB: ~w~n", [dbgindent(StopsVisited), TripsRidingThruStopB]),

  TripsWalkingToB = [
    [{mins_of_mode_switch(StopAId, TransitModeToA, walk, 0),
      walk,
      DistanceAB / ?WALK_METERS_PER_MIN, % travel mins
      StopB#stop.id} |
      optimal_trip(Tabs,
                   walk,
                   WalkedMins + (DistanceAB / ?WALK_METERS_PER_MIN),
                   StopB#stop.id,
                   StopZId,
                   State#tripstate{stopsVisited=[StopAId]++StopsVisited})] ||
    {StopB, DistanceAB} <- stops_walkable_from_stop(StopsTab, StopAId, [StopAId] ++ StopsVisited, MaxWalkMinsAllowed)
  ],
  io:fwrite("~sTripsWalkingToB: ~w~n", [dbgindent(StopsVisited), TripsWalkingToB]),

  % Direct walking (as the crow flies) is an option too...
  [StopA, StopZ] = transit_data:stop(StopsTab, [StopAId, StopZId]),
  DirectRouteWalkMins = person:direct_walk_mins(StopA, StopZ),

  AllPossibleTrips =
    if DirectRouteWalkMins =< MaxWalkMinsAllowed ->
        DirectRoute = [[{0, walk, DirectRouteWalkMins, StopZId}]],
        TripsRidingThruStopB ++ TripsWalkingToB ++ DirectRoute;
      true ->
        TripsRidingThruStopB ++ TripsWalkingToB
    end,
  io:fwrite("~sAllPossibleTrips: ~w~n", [dbgindent(StopsVisited), AllPossibleTrips]),

  OptimalTrip = min_by(AllPossibleTrips, fun total_mins/1),
  io:fwrite("~soptimal_trip(~w, ~w, ~w, ~w) => ~w~n", [dbgindent(StopsVisited), StopsVisited, TransitModeToA, StopAId, StopZId, OptimalTrip]),
  OptimalTrip.

trip_test_optimal_trip_AB(Tabs) ->
  ?assertMatch(
    [
      {0, walk, WalkTime, stopB}
    ] when WalkTime > 7 andalso WalkTime < 8,
    optimal_trip(Tabs, walk, 0, stopA, stopB)
  ).

trip_test_optimal_trip_AC(Tabs) ->
  ?assertMatch(
    [
      % too tired to walk the whole way, so we'll catch the Yellow Line one stop
      {0, walk, WalkTime, stopB},
      {5, routeYellow, 3, stopC}
    ] when WalkTime > 7 andalso WalkTime < 8,
    optimal_trip(Tabs, walk, 0, stopA, stopC)
  ).

trip_test_optimal_trip_AF(Tabs) ->
  ?assertMatch(
    [
      {0, walk, WalkTime, stopB},
      {5, routeYellow, 3, stopC},
      {3, routeGreen, 3, stopF}
    ] when WalkTime > 7 andalso WalkTime < 8,
    optimal_trip(Tabs, walk, 0, stopA, stopF)
  ).

route_time(Route) ->
  lists:map(
    fun({WaitTime, _, TravelTime, _}) -> WaitTime + TravelTime end,
    Route
  ).

fastest_trip(Tabs, FromCoords, ToCoords) ->
  % Algorithm:
  % Let FromStops be the set of stops walkable from FromCoords.
  % Let ToStops be the set of stops walkable to ToCoords.
  % For every segment in the Cartesian product of FromStops and ToStops,
  %   find the optimal route of the segment
  % Return the time of the fastest optimal route.
  {stops, StopsTab} = lists:nth(1, ets:lookup(Tabs, stops)),
  FromStops = stops_walkable_from_coords(StopsTab, FromCoords, ?MAX_WALK_MINS),
  ToStops = stops_walkable_from_coords(StopsTab, ToCoords, ?MAX_WALK_MINS),
  io:fwrite("fastest_trip: FromStops=~w~n", [FromStops]),
  io:fwrite("fastest_trip: ToStops=~w~n", [ToStops]),
  OptimalRoutes = [
    optimal_trip(Tabs,
                 walk,
                 FromStopMeters / ?WALK_METERS_PER_MIN,
                 FromStop,
                 ToStop) ||
    {FromStop, FromStopMeters} <- FromStops,
    {ToStop, _ToStopMeters} <- ToStops
  ],
  min_by(OptimalRoutes, fun(Route) -> route_time(Route) end).

trip_test_fastest_trip(Tabs) ->
  FromCoords = #coords{lat=47, lon=-122},
  ToCoords = #coords{lat=47.032, lon=-122},
  Trip = fastest_trip(Tabs, FromCoords, ToCoords),
  ?assertEqual(1, 2).

%%%%%%%%%
% TESTING

trip_test_() ->
  {
    setup,
    fun test_data:setup_transit_data/0,
    fun test_data:teardown_transit_data/1,
    {with, [
      fun trip_test_stops_walkable_from_coords/1,
      fun trip_test_connections_from_stop_1/1,
      fun trip_test_connections_from_stop_2/1,
      fun trip_test_optimal_trip_AB/1,
      fun trip_test_optimal_trip_AC/1,
      fun trip_test_optimal_trip_AF/1,
      fun trip_test_stops_walkable_from_stop/1,
      fun trip_test_fastest_trip/1
    ]}
  }.
