-module(trip).
-export([
  connections_from_stop_with_txfr/3,
  connections_from_stop_without_txfr/3,
  optimal_trip_between_coords/4,
  optimal_trip_to_stop/2,
  segs_total_secs/1,
  stops_walkable_from_coords/3,
  stops_walkable_from_stop/4,
  total_walk_secs/1]).

-include("records/sconn.hrl").
-include("records/stop.hrl").
-include("person.hrl").
-include("test_data.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(trip_config, {
  tabs,
  log,
  stopZid,
  totalSecsAllowed,
  totalTMChangesAllowed
}).

-record(trip_result, {
  optimalTrip,
  fnInstQty
}).

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

% At a stop, the cost of switching from transit mode A to B, expressed in Wait seconds.
secs_of_mode_switch(_ModeA, _ModeA, _W) -> 0; % no switch
secs_of_mode_switch(_ModeA, walk, _W) -> 0; % get off the bus and walk
secs_of_mode_switch(walk, _ModeB, W) -> W; % get on the bus X
secs_of_mode_switch(_ModeA, _ModeB, W) -> W. % transfer to bus X

secs_of_mode_switch_test() ->
  ?assertEqual(
    3,
    secs_of_mode_switch(routeBC, routeCD, 3)
  ).

total_walk_secs(Trip) -> total_walk_secs(Trip, 0).

total_walk_secs([], Mins) -> Mins;
total_walk_secs([{_WaitSecs, walk, WalkSecs, _ToStop}|Rest], WalkedSecsSoFar) ->
  total_walk_secs(Rest, WalkedSecsSoFar+WalkSecs);
total_walk_secs([{_WaitSecs, _TransitMode, _TravelSecs, _ToStop}|Rest], WalkedSecsSoFar) ->
  total_walk_secs(Rest, WalkedSecsSoFar).

total_walk_secs_test() ->
  ?assertMatch(8, total_walk_secs([
    {0, walk, 5, stopA},
    {4, someBus, 10, stopB},
    {0, walk, 3, stopC}
  ])).

connections_from_stop_with_txfr(SConnsTab, StopAId, StopIdsToExclude) ->
  AllSConns = ets:lookup(SConnsTab, StopAId),
  lists:filter(
    fun (SConn) -> not sets:is_element(SConn#sconn.to_stop_id, StopIdsToExclude) end,
    AllSConns).

trip_test_connections_from_stop_with_txfr_1(Tabs) ->
  [{sconns, SConnsTab}] = ets:lookup(Tabs, sconns),
  ?assertEqual(
    [?TEST_SCONN_YELLOW_C_D, ?TEST_SCONN_GREEN_C_F],
    connections_from_stop_with_txfr(SConnsTab, stopC, sets:new())
  ).

trip_test_connections_from_stop_with_txfr_2(Tabs) ->
  [{sconns, SConnsTab}] = ets:lookup(Tabs, sconns),
  ?assertEqual(
    [?TEST_SCONN_GREEN_C_F],
    connections_from_stop_with_txfr(SConnsTab, stopC, sets:from_list([stopD]))
  ).

connections_from_stop_without_txfr(SConnsTab, StopAId, TransitModeToA) ->
  AllSConns = ets:lookup(SConnsTab, StopAId),
  lists:filter(
    fun (SConn) -> SConn#sconn.transit_mode == TransitModeToA end,
    AllSConns).

trip_test_connections_from_stop_without_txfr_1(Tabs) ->
  [{sconns, SConnsTab}] = ets:lookup(Tabs, sconns),
  ?assertEqual(
    [?TEST_SCONN_BLUE_E_G],
    connections_from_stop_without_txfr(SConnsTab, stopE, routeBlue)
  ).

% Returns a list in the format:
%   [ {StopAId, MetersDistance}, {StopBId, MetersDistance}, ... ]
stops_walkable_from_coords(StopsTab, FromCoords, MaxWalkSecsAllowed) ->
  Distances = transit_data:ets_map(StopsTab, fun(StopX) ->
    {StopX, earth:meters_between_coords(FromCoords, StopX#stop.coords)} end),
  lists:filter(
    fun({_StopX, MetersAway}) ->
      (MetersAway < MaxWalkSecsAllowed*?WALK_METERS_PER_SEC)
    end,
    Distances).

trip_test_stops_walkable_from_coords(Tabs) ->
  [{stops, StopsTab}] = ets:lookup(Tabs, stops),
  ?assertMatch(
    [
      {?TEST_STOP_C, DistanceC},
      {?TEST_STOP_B, DistanceB},
      {?TEST_STOP_A, DistanceA}
    ] when DistanceA > 60*2 andalso DistanceA < 60*3
      andalso DistanceB > 60*10 andalso DistanceB < 60*11
      andalso DistanceC > 60*18 andalso DistanceC < 60*19,
      stops_walkable_from_coords(StopsTab, #coords{lat=47, lon=-122}, ?MAX_WALK_SECS_TOTAL)
  ).

% All stops surrounding the input stop, excluding the given set of stops.
% Returns a list in the format:
%   [ {StopAId, MetersDistance}, {StopBId, MetersDistance}, ... ]
stops_walkable_from_stop(StopsTab, FromStopId, StopIdsExcluded, MaxWalkSecsAllowed) ->
  [FromStop] = transit_data:stop(StopsTab, [FromStopId]),
  lists:filter(
    fun({StopX, _}) ->
      (not sets:is_element(StopX#stop.id, StopIdsExcluded))
    end,
    stops_walkable_from_coords(StopsTab, FromStop#stop.coords, MaxWalkSecsAllowed)).

trip_test_stops_walkable_from_stop(Tabs) ->
  [{stops, StopsTab}] = ets:lookup(Tabs, stops),
  ?assertMatch(
    [{?TEST_STOP_B, Distance}] when Distance > 480 andalso Distance < 481,
      stops_walkable_from_stop(StopsTab, stopA, sets:from_list([stopA]), ?MAX_WALK_SECS_TO_NEXT_STOP)
  ).

-spec segs_total_secs([{number(), _, number(), _}]) -> number().
segs_total_secs(Trip) -> lists:foldl(
  fun ({ModeSwitchSecs, _TM, TransitSecs, _StopB}, AccIn) -> ModeSwitchSecs + TransitSecs + AccIn end,
  0,
  Trip).

total_secs_of_trip_result(TR) -> segs_total_secs(TR#trip_result.optimalTrip).

total_secs_of_trip_result_test() ->
  ?assertEqual(13,
    total_secs_of_trip_result(
      #trip_result{
        optimalTrip = [
          {0, walk, 5, stopA},
          {5, route5, 3, stopB}
        ]})).

dbgindent(QtyStopsVisited) ->
  io_lib:format("(~B stops visited) ", [QtyStopsVisited]).
%%  string:chars($., 3*QtyStopsVisited).

segs_walked(L) -> lists:filter(fun({_, Mode, _, _}) -> Mode == walk end, L).

segs_walked_total_secs(L) -> segs_walked_total_secs(L, 0).
segs_walked_total_secs([], Sum) -> Sum;
segs_walked_total_secs([{T1, walk, T2, _}|Tail], Sum) ->
  segs_walked_total_secs(Tail, Sum + T1 + T2);
segs_walked_total_secs([_|Tail], Sum) ->
  segs_walked_total_secs(Tail, Sum).

segs_stop_ids(Segs) -> segs_stop_ids(Segs, sets:new()).
segs_stop_ids([], StopIdsVisited) -> StopIdsVisited;
segs_stop_ids([{_, _, _, StopId}|Tail], StopIdsVisited) ->
  segs_stop_ids(Tail, sets:add_element(StopId, StopIdsVisited)).

segs_stop_ids_test() ->
  ?assertEqual(sets:from_list([stopA, stopB]), segs_stop_ids([
    {0, walk, 0, stopA},
    {3, route5, 10, stopB}
  ])).

segs_transit_mode_changes([]) -> 0;
segs_transit_mode_changes([{_, TransitMode, _, _}|TL]) -> segs_transit_mode_changes(TL, TransitMode, 0).
segs_transit_mode_changes([], _, TransfersQty) -> TransfersQty;
segs_transit_mode_changes([{_, TransitMode, _, _}|TL], PrevTransitMode, TransfersQty) ->
  if TransitMode == PrevTransitMode ->
    segs_transit_mode_changes(TL, TransitMode, TransfersQty);
    true ->
      segs_transit_mode_changes(TL, TransitMode, TransfersQty+1)
  end.

segs_transit_mode_changes_test() ->
  ?assertEqual(3, segs_transit_mode_changes([
    {0, walk, 0, stopA},
    {0, bus60, 0, stopB},
    {0, bus60, 0, stopC},
    {0, bus5, 0, stopD},
    {0, walk, 0, dest}
  ])).

% If we are in the middle of a route (walking to this stop wasn't our first segment), and
% we walked here, then we know that walking to another stop will not be an optimal route.
% Proof by contradiction: suppose walking X->Y->Z is optimal.  Then it must be that path
% X-Y-Z is shorter than X-Z directly.  That is impossible.
segs_walking_is_reasonable([]) -> true;
segs_walking_is_reasonable([_]) -> true;
segs_walking_is_reasonable([_, _]) -> true;
segs_walking_is_reasonable(L) ->
  [{_, TM1, _, _}, {_, TM2, _, _}] = lists:sublist(L, 2),
  TM1 /= walk orelse TM2 /= walk.

segs_walking_is_reasonable_1_test() ->
  ?assertEqual(true, segs_walking_is_reasonable([
    {10, route5, 100, stopB},
    {0, walk, 0, stopA}])).

segs_walking_is_reasonable_2_test() ->
  ?assertEqual(false, segs_walking_is_reasonable([
    {0, walk, 100, stopD},
    {0, walk, 100, stopC},
    {10, route5, 100, stopB},
    {0, walk, 0, stopA}])).

% Given an initial sequence of transit segments that end on stop A,
% find optimal trip to stop Z and return the new travel segments in
% reverse chronological order.
%
% This function will always find a route, even if the route is
% simply walking for fourscore and seven days.
%
% The function requires at least one initial segment.
%
% A travel segment is a 4-tuple:
%   {<mode-switch-or-wait-secs>, <transit-mode>, <transit-secs>, <to-stop-X>}
%
% Transit modes: walk | RouteId
%
% Base case:
%   We arrived at Stop Z.  (Stop A = Stop Z)
% Inductive cases:
%   a) Find a <<stops connection>> from Stop A to Stop B, then
%      solve the sub-problem of Optimal Route from B to Z.
%   b) Walk from Stop A to some Stop B within walking distance of A,
%      then solve the sub-problem of Optimal Route from B to Z.
%
% Example input:
%   [ {0-mins-wait, Route40, 5-mins-transit, to-StopA} ]
%
% Example output:
% [
%   {3-mins-wait, Route88, 10-mins-transit, to-StopC},
%   {0-mins-wait, Route40, 5-mins-transit, to-StopB}
% ]
%
optimal_trip_to_stop(TripConfig, InitSegs) ->
%%  io:fwrite("~soptimal_trip_to_stop(~p, ~p) invoked...~n",
%%    [dbgindent(length(InitSegs)), InitSegs, StopZId]),

  [{_, TransitModeToA, _, StopAId}|_] = InitSegs,

  WalkingSegmentsQty = length(segs_walked(InitSegs)),
  TotalTripSecs = segs_total_secs(InitSegs),
  WalkedSecs = segs_walked_total_secs(InitSegs),
  StopIdsVisited = segs_stop_ids(InitSegs),
  TransitModeChangeQty = segs_transit_mode_changes(InitSegs),

  % optimizations
  CanContinueTrip = TotalTripSecs < TripConfig#trip_config.totalSecsAllowed
    andalso WalkingSegmentsQty =< (TransitModeChangeQty +2)
    andalso segs_walking_is_reasonable(InitSegs),
  CanChangeTransitMode = TransitModeChangeQty < TripConfig#trip_config.totalTMChangesAllowed,

  [{stops, StopsTab}] = ets:lookup(TripConfig#trip_config.tabs, stops),
  [{sconns, SConnsTab}] = ets:lookup(TripConfig#trip_config.tabs, sconns),

  % Now suppose we have a set of Stops B, reachable from stop A via some transit-mode.

  % We are assembling a list of:
  %   for each connection from StopA to StopB,
  %     the optimal route by taking that connection.
  % Thus, the `transit-mode` is unique for each element in this list.
  % [
  %   [{<mode-switch-mins>, <transit-mode>, <transit-time>, <to-stop-X>}, [...optimal-route-from-X]]
  %   [{3 min, Route 40, 2 min, Stop40040}, [...optimal route from Stop40040]],
  %   [{5 min, Route 28, 3 min, Stop28028}, [...optimal route from Stop28028]],
  % ]

  SConnsRidingThruStopB =
    if
      CanContinueTrip andalso CanChangeTransitMode ->
        connections_from_stop_with_txfr(SConnsTab, StopAId, sets:add_element(StopAId, StopIdsVisited));
      CanContinueTrip ->
        connections_from_stop_without_txfr(SConnsTab, StopAId, TransitModeToA);
      true ->
        []
    end,
  TripResultsRidingThruStopB = parallel:parmap_all(
    fun(SConn) ->
    optimal_trip_to_stop(
      TripConfig,
      [{
        secs_of_mode_switch(TransitModeToA, SConn#sconn.transit_mode, SConn#sconn.wait_secs),
        SConn#sconn.transit_mode,
        SConn#sconn.travel_secs,
        SConn#sconn.to_stop_id}] ++ InitSegs
    )
    end, SConnsRidingThruStopB),

  MaxWalkSecsAllowed = min(?MAX_WALK_SECS_TO_NEXT_STOP, ?MAX_WALK_SECS_TOTAL - WalkedSecs),
  StopsWalkableFromStop =
    if
      CanContinueTrip andalso CanChangeTransitMode ->
        stops_walkable_from_stop(StopsTab, StopAId, sets:add_element(StopAId, StopIdsVisited), MaxWalkSecsAllowed);
      true -> [] end,
  TripResultsWalkingToStopB = parallel:parmap_firstN(
    20,
    fun({StopB, DistanceAB}) ->
      optimal_trip_to_stop(
        TripConfig,
        [{
          secs_of_mode_switch(TransitModeToA, walk, 0),
          walk,
          DistanceAB / ?WALK_METERS_PER_SEC, % travel time in seconds
          StopB#stop.id}] ++ InitSegs
      )
    end, StopsWalkableFromStop),

  % The most basic option is direct walking (as the crow flies), bypassing stops,
  % unless we are already here.
  StopZId = TripConfig#trip_config.stopZid,
  DirectWalkTrip =
    if StopAId /= StopZId ->
      [StopA, StopZ] = transit_data:stop(StopsTab, [StopAId, StopZId]),
      [{0, walk, person:direct_walk_secs(StopA, StopZ), StopZId}] ++ InitSegs;
      true -> InitSegs % don't move
    end,
%%  io:fwrite("~sDirectWalkTrip: ~w~n", [dbgindent(length(InitSegs)), DirectWalkTrip]),

  AllPossibleTrips =
    [DirectWalkTrip]
    ++
    lists:map(fun (TR) -> TR#trip_result.optimalTrip end, TripResultsRidingThruStopB)
    ++
    lists:map(fun (TR) -> TR#trip_result.optimalTrip end, TripResultsWalkingToStopB),
%%  io:fwrite("~sAllPossibleTrips: ~w~n", [dbgindent(length(InitSegs)), AllPossibleTrips]),

  SumFnInstQty = lists:sum(
    [1] % one for me
    ++
    lists:map(fun (TR) -> TR#trip_result.fnInstQty end, TripResultsRidingThruStopB)
    ++
    lists:map(fun (TR) -> TR#trip_result.fnInstQty end, TripResultsWalkingToStopB)),

  OptimalTrip = min_by(AllPossibleTrips, fun segs_total_secs/1),

  if TripConfig#trip_config.log /= undefined ->
    ok = disk_log:alog(TripConfig#trip_config.log, {
      optimal_trip_to_stop,
      finished,
      {initSegs, InitSegs},
      {transitModeToA, TransitModeToA},
      {stopAID, StopAId},
      {stopZID, TripConfig#trip_config.stopZid},
      {totalTripSecs, TotalTripSecs},
      {walkedSecs, WalkedSecs},
      {stopIdsVisited, StopIdsVisited},
      {transfersQty, TransitModeChangeQty},
      {canContinueTrip, CanContinueTrip},
      {canTransfer, CanChangeTransitMode},
      {sConnsRidingThruStopB, length(SConnsRidingThruStopB)},
      {stopsWalkableFromStop, length(StopsWalkableFromStop)},
      {optimalTrip, OptimalTrip},
      {fnInstQty, SumFnInstQty}});
    true -> ok end,

  #trip_result{
    optimalTrip = OptimalTrip,
    fnInstQty = SumFnInstQty
  }.

trip_test_optimal_trip_to_stop_AB(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, WalkSecs, stopB},
        {0, walk, 0, stopA}
  ],
      fnInstQty = 101
    } when WalkSecs > 60*7 andalso WalkSecs < 60*9,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopB, totalTMChangesAllowed = 5},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_AD(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        % too tired to walk the whole way, so we'll catch the Yellow Line two stops
        {0, routeYellow, 60*3, stopD},
        {60*3, routeYellow, 60*3, stopC},
        {0, walk, WalkSecs, stopB},
        {0, walk, 0, stopA}
      ],
      fnInstQty = 101
    } when WalkSecs > 60*7 andalso WalkSecs < 60*9,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopD, totalTMChangesAllowed = 5},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_AF(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {60*3, routeGreen, 60*3, stopF},
        {60*3, routeYellow, 60*3, stopC},
        {0, walk, WalkSecs, stopB},
        {0, walk, 0, stopA}
      ],
      fnInstQty = 101
    } when WalkSecs > 60*7 andalso WalkSecs < 60*9,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopF, totalTMChangesAllowed = 5},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_AZ(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, WalkSecsFZ, stopZ},
        {60*3, routeGreen, 60*3, stopF},
        {60*3, routeYellow, 60*3, stopC},
        {0, walk, WalkSecsAB, stopB},
        {0, walk, 0, stopA}
      ],
      fnInstQty = 99
    } when WalkSecsAB > 60*7 andalso WalkSecsAB < 60*9 andalso
      WalkSecsFZ > 60*60 andalso WalkSecsFZ < 60*61,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopZ, totalSecsAllowed = 10000, totalTMChangesAllowed = 10},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_AZ_limitedModeChg(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, WalkSecsFZ, stopZ},
        {0, routeYellow, 60*3, stopF},
        {0, routeYellow, 60*3, stopE},
        {0, routeYellow, 60*3, stopD},
        {60*3, routeYellow, 60*3, stopC},
        {0, walk, WalkSecsAB, stopB},
        {0, walk, 0, stopA}
      ],
      fnInstQty = 8
    } when WalkSecsAB > 60*7 andalso WalkSecsAB < 60*9 andalso
      WalkSecsFZ > 60*60 andalso WalkSecsFZ < 60*61,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopZ, totalSecsAllowed = 10000, totalTMChangesAllowed = 1},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_ZA(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0,walk,WalkSecsBA,stopA},
        {0,routeBlue,120,stopB},
        {0,routeBlue,180,stopI},
        {0,routeBlue,120,stopH},
        {0,routeBlue,120,stopG},
        {0,routeBlue,120,stopE},
        {180,routeBlue,300,stopF},
        {0, walk, 0, stopZ}
      ],
      fnInstQty = 40
    } when WalkSecsBA > 60*7 andalso WalkSecsBA < 60*9,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopA, totalSecsAllowed = 10000, totalTMChangesAllowed = 10},
        [{0, walk, 0, stopZ}])).

trip_test_optimal_trip_to_stop_tooMuchWalking(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, WalkSecsBZ, stopZ},
        {0, walk, 100, stopC},
        {0, walk, 100, stopB},
        {0, walk, 0, stopA}
      ],
      fnInstQty = 1
    } when WalkSecsBZ > 60*84 andalso WalkSecsBZ < 60*85,
    optimal_trip_to_stop(
      #trip_config{tabs = Tabs, stopZid = stopZ, totalSecsAllowed = 10000, totalTMChangesAllowed = 10},
      [
        {0, walk, 100, stopC},
        {0, walk, 100, stopB},
        {0, walk, 0, stopA}
      ]
    )).

optimal_trip_between_coords(Tabs, FromCoords, ToCoords, TotalModeChangesAllowed) ->
  % Algorithm:
  % Let FromStops be the set of stops walkable from FromCoords.
  % Let ToStops be the set of stops walkable to ToCoords.
  % For every pair of stops in the Cartesian product of FromStops and ToStops,
  %   find the optimal route.
  % Return the time of the fastest optimal route.
  [{stops, StopsTab}] = ets:lookup(Tabs, stops),
  FromStops = stops_walkable_from_coords(StopsTab, FromCoords, ?MAX_WALK_SECS_TO_NEXT_STOP),
  ToStops = stops_walkable_from_coords(StopsTab, ToCoords, ?MAX_WALK_SECS_TO_NEXT_STOP),
  io:fwrite("optimal_trip_between_coords: FromStops=~p ToStops=~p~n", [FromStops, ToStops]),

  TripsPlusFinalWalks = [
    {
      optimal_trip_to_stop(
        #trip_config{
          tabs = Tabs,
          stopZid = ToStop#stop.id,
          totalTMChangesAllowed = TotalModeChangesAllowed
        },
        [{0, walk, FromStopMeters / ?WALK_METERS_PER_SEC, FromStop#stop.id}]
      ),
      {0, walk, earth:meters_between_coords(ToStop#stop.coords, ToCoords) / ?WALK_METERS_PER_SEC, to_dest_coords}
    } ||
    {FromStop, FromStopMeters} <- FromStops,
    {ToStop, _ToStopMeters} <- ToStops
  ],
  OptimalTripResults = lists:map(
    fun ({TripResult, FinalWalk}) -> #trip_result{
      optimalTrip = [FinalWalk] ++ TripResult#trip_result.optimalTrip,
      fnInstQty = TripResult#trip_result.fnInstQty
    } end,
    TripsPlusFinalWalks),
%%  io:fwrite("OptimalTripResults: ~p~n", [OptimalTripResults]),
  min_by(OptimalTripResults, fun total_secs_of_trip_result/1).

trip_test_optimal_trip_between_coords(Tabs) ->
  FromCoords = #coords{lat=47.001, lon=-122}, % stop A is closer than stop B
  ToCoords = #coords{lat=47.023, lon=-122},
  TripResult = optimal_trip_between_coords(Tabs, FromCoords, ToCoords, 4),
  % from-stops: {A, B}.  to-stops: {E, F}.
  % walk to stop B, wait 5 minutes for yellow route, then take it to stop E.
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, WalkSecsFromF, to_dest_coords},
        {60*3, routeGreen, 60*3, stopF},
        {60*3, routeYellow, 60*3, stopC},
        {0, walk, WalkSecsToB, stopB}
      ],
      fnInstQty = 47
    } when WalkSecsToB > 60*9 andalso WalkSecsToB < 60*10
      andalso WalkSecsFromF > 0 andalso WalkSecsFromF < 60*1,
    TripResult).

%%%%%%%%%
% TESTING

trip_test_() ->
  {
    setup,
    fun test_data:setup_transit_data/0,
    fun test_data:teardown_transit_data/1,
    {with, [
      fun trip_test_stops_walkable_from_coords/1,
      fun trip_test_connections_from_stop_with_txfr_1/1,
      fun trip_test_connections_from_stop_with_txfr_2/1,
      fun trip_test_connections_from_stop_without_txfr_1/1,
      fun trip_test_optimal_trip_to_stop_AB/1,
      fun trip_test_optimal_trip_to_stop_AD/1,
      fun trip_test_optimal_trip_to_stop_AF/1,
      fun trip_test_optimal_trip_to_stop_AZ/1,
      fun trip_test_optimal_trip_to_stop_AZ_limitedModeChg/1,
      fun trip_test_optimal_trip_to_stop_ZA/1,
      fun trip_test_optimal_trip_to_stop_tooMuchWalking/1,
      fun trip_test_stops_walkable_from_stop/1,
      fun trip_test_optimal_trip_between_coords/1
    ]}
  }.
