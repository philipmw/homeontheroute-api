-module(trip).
-export([
  connections_from_stop_with_txfr/3,
  connections_from_stop_without_txfr/3,
  optimal_trip_between_coords/4,
  optimal_trip_to_stop/2,
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
  totalTxfrAllowed
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
      % These are out of order in real data, and I don't care enough
      % to make the test agnostic of ordering.
      {?TEST_STOP_B, DistanceB},
      {?TEST_STOP_A, DistanceA}
    ] when DistanceA > 111 andalso DistanceA < 112
      andalso DistanceB > 778 andalso DistanceB < 779,
      stops_walkable_from_coords(StopsTab, #coords{lat=47, lon=-122}, ?MAX_WALK_SECS_TO_NEXT_STOP)
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
    [{?TEST_STOP_B, Distance}] when Distance > 667 andalso Distance < 668,
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

segs_transfers([]) -> 0;
segs_transfers([{_, TransitMode, _, _}|TL]) -> segs_transfers(TL, TransitMode, 0).
segs_transfers([], _, TransfersQty) -> TransfersQty;
segs_transfers([{_, TransitMode, _, _}|TL], PrevTransitMode, TransfersQty) ->
  if TransitMode == PrevTransitMode ->
    segs_transfers(TL, TransitMode, TransfersQty);
    true ->
      segs_transfers(TL, TransitMode, TransfersQty+1)
  end.

segs_transfers_test() ->
  ?assertEqual(3, segs_transfers([
    {0, walk, 0, stopA},
    {0, bus60, 0, stopB},
    {0, bus60, 0, stopC},
    {0, bus5, 0, stopD},
    {0, walk, 0, dest}
  ])).

% Given an initial sequence of transit segments that end on stop A,
% find optimal trip to stop Z and return the new travel segments.
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
%   {0-mins-wait, Route40, 5-mins-transit, to-StopB},
%   {3-mins-wait, Route88, 10-mins-transit, to-StopC}
% ]
%
optimal_trip_to_stop(TripConfig, InitSegs) ->
%%  io:fwrite("~soptimal_trip_to_stop(~p, ~p) invoked...~n",
%%    [dbgindent(length(InitSegs)), InitSegs, StopZId]),

  {_, TransitModeToA, _, StopAId} = lists:last(InitSegs),

  WalkingSegmentsQty = length(segs_walked(InitSegs)),
  TotalTripSecs = segs_total_secs(InitSegs),
  WalkedSecs = segs_walked_total_secs(InitSegs),
  StopIdsVisited = segs_stop_ids(InitSegs),
  TransfersQty = segs_transfers(InitSegs),

  % optimizations
  CanContinueTrip = TotalTripSecs < TripConfig#trip_config.totalSecsAllowed andalso WalkingSegmentsQty =< (TransfersQty+2),
  CanTransfer = TransfersQty < TripConfig#trip_config.totalTxfrAllowed,

  if TripConfig#trip_config.log /= undefined ->
    ok = disk_log:alog(TripConfig#trip_config.log, {
      optimal_trip_to_stop,
      invoked,
      {initSegs, InitSegs},
      {transitModeToA, TransitModeToA},
      {stopAID, StopAId},
      {stopZID, TripConfig#trip_config.stopZid},
      {totalTripSecs, TotalTripSecs},
      {walkedSecs, WalkedSecs},
      {stopIdsVisited, StopIdsVisited},
      {transfersQty, TransfersQty},
      {canContinueTrip, CanContinueTrip},
      {canTransfer, CanTransfer}});
    true -> ok end,

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
      CanContinueTrip andalso CanTransfer ->
        connections_from_stop_with_txfr(SConnsTab, StopAId, sets:add_element(StopAId, StopIdsVisited));
      CanContinueTrip ->
        connections_from_stop_without_txfr(SConnsTab, StopAId, TransitModeToA);
      true ->
        []
    end,
  TripResultsRidingThruStopB = lists:map(fun(SConn) ->
    optimal_trip_to_stop(
      TripConfig,
      InitSegs ++ [{
        secs_of_mode_switch(TransitModeToA, SConn#sconn.transit_mode, SConn#sconn.wait_secs),
        SConn#sconn.transit_mode,
        SConn#sconn.travel_secs,
        SConn#sconn.to_stop_id}]
    )
    end, SConnsRidingThruStopB),
%%  TripResultsRidingThruStopB = [
%%    optimal_trip_to_stop(
%%      TripConfig,
%%      InitSegs ++ [{
%%        secs_of_mode_switch(TransitModeToA, SConn#sconn.transit_mode, SConn#sconn.wait_secs),
%%        SConn#sconn.transit_mode,
%%        SConn#sconn.travel_secs,
%%        SConn#sconn.to_stop_id}]
%%    ) || SConn <- SConnsRidingThruStopB],
%%  io:fwrite("~sTripsRidingThruStopB: ~p~n", [dbgindent(length(InitSegs)), TripsRidingThruStopB]),

  MaxWalkSecsAllowed = min(?MAX_WALK_SECS_TO_NEXT_STOP, ?MAX_WALK_SECS_TOTAL - WalkedSecs),
  StopsWalkableFromStop =
    if CanTransfer -> stops_walkable_from_stop(StopsTab, StopAId, sets:add_element(StopAId, StopIdsVisited), MaxWalkSecsAllowed);
      true -> [] end,
  TripResultsWalkingToStopB = parallel:parmap(fun({StopB, DistanceAB}) ->
    optimal_trip_to_stop(
      TripConfig,
      InitSegs ++ [{
        secs_of_mode_switch(TransitModeToA, walk, 0),
        walk,
        DistanceAB / ?WALK_METERS_PER_SEC, % travel time in seconds
        StopB#stop.id}]) end, StopsWalkableFromStop),

%%  TripResultsWalkingToStopB = [
%%    optimal_trip_to_stop(
%%      TripConfig,
%%      InitSegs ++ [{
%%        secs_of_mode_switch(TransitModeToA, walk, 0),
%%        walk,
%%        DistanceAB / ?WALK_METERS_PER_SEC, % travel time in seconds
%%        StopB#stop.id}]
%%    ) || {StopB, DistanceAB} <- StopsWalkableFromStop],
%%  io:fwrite("~sTripsWalkingToB: ~p~n", [dbgindent(length(InitSegs)), TripsWalkingToB]),

  % The most basic option is direct walking (as the crow flies), bypassing stops,
  % unless we are already here.
  StopZId = TripConfig#trip_config.stopZid,
  DirectWalkTrip =
    if StopAId /= StopZId ->
      [StopA, StopZ] = transit_data:stop(StopsTab, [StopAId, StopZId]),
      InitSegs ++ [{0, walk, person:direct_walk_secs(StopA, StopZ), StopZId}];
      true -> InitSegs % don't move
    end,
%%  io:fwrite("~sDirectWalkTrip: ~w~n", [dbgindent(length(InitSegs)), DirectWalkTrip]),

  AllPossibleTrips =
    lists:map(fun (TR) -> TR#trip_result.optimalTrip end, TripResultsRidingThruStopB)
    ++
    lists:map(fun (TR) -> TR#trip_result.optimalTrip end, TripResultsWalkingToStopB)
    ++
    [DirectWalkTrip],
%%  io:fwrite("~sAllPossibleTrips: ~w~n", [dbgindent(length(InitSegs)), AllPossibleTrips]),

  SumFnInstQty = lists:sum(
    lists:map(fun (TR) -> TR#trip_result.fnInstQty end, TripResultsRidingThruStopB)
    ++
    lists:map(fun (TR) -> TR#trip_result.fnInstQty end, TripResultsWalkingToStopB)
    ++
    [1]), % one for me

  OptimalTrip = min_by(AllPossibleTrips, fun segs_total_secs/1),

  if SumFnInstQty >= 100000 ->
    io:fwrite("~soptimal_trip_to_stop(~p, ~p) => ~p (~B fnInstQty)~n",
      [dbgindent(length(InitSegs)), InitSegs, StopZId, OptimalTrip, SumFnInstQty]);
    true -> ok end,

  #trip_result{
    optimalTrip = OptimalTrip,
    fnInstQty = SumFnInstQty
  }.

trip_test_optimal_trip_to_stop_AB(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, 0, stopA},
        {0, walk, WalkSecs, stopB}
      ],
      fnInstQty = 21
    } when WalkSecs > 60*7 andalso WalkSecs < 60*8,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopB, totalTxfrAllowed = 2},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_AD(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        % too tired to walk the whole way, so we'll catch the Yellow Line two stops
        {0, walk, 0, stopA},
        {0, walk, WalkSecs, stopB},
        {60*3, routeYellow, 60*3, stopC},
        {0, routeYellow, 60*3, stopD}
      ],
      fnInstQty = 21
    } when WalkSecs > 60*7 andalso WalkSecs < 60*8,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopD, totalTxfrAllowed = 2},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_AF(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, 0, stopA},
        {0, walk, WalkSecs, stopB},
        {60*3, routeYellow, 60*3, stopC},
        {60*3, routeGreen, 60*3, stopF}
      ],
      fnInstQty = 21
    } when WalkSecs > 60*7 andalso WalkSecs < 60*8,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopF, totalTxfrAllowed = 2},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_AZ(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, 0, stopA},
        {0, walk, WalkSecsAB, stopB},
        {60*3, routeYellow, 60*3, stopC},
        {60*3, routeGreen, 60*3, stopF},
        {0, walk, WalkSecsFZ, stopZ}
      ],
      fnInstQty = 108
    } when WalkSecsAB > 60*7 andalso WalkSecsAB < 60*8 andalso
      WalkSecsFZ > 60*60 andalso WalkSecsFZ < 60*61,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopZ, totalSecsAllowed = 10000, totalTxfrAllowed = 10},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_AZ_limitedTxfr(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, 0, stopA},
        {0, walk, WalkSecsAB, stopB},
        {60*3, routeYellow, 60*3, stopC},
        {0, routeYellow, 60*3, stopD},
        {0, routeYellow, 60*3, stopE},
        {0, routeYellow, 60*3, stopF},
        {0, walk, WalkSecsFZ, stopZ}
      ],
      fnInstQty = 8
    } when WalkSecsAB > 60*7 andalso WalkSecsAB < 60*8 andalso
      WalkSecsFZ > 60*60 andalso WalkSecsFZ < 60*61,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopZ, totalSecsAllowed = 10000, totalTxfrAllowed = 1},
        [{0, walk, 0, stopA}])).

trip_test_optimal_trip_to_stop_ZA(Tabs) ->
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, 0, stopZ},
        {180,routeBlue,300,stopF},
        {0,routeBlue,120,stopE},
        {0,routeBlue,120,stopG},
        {0,routeBlue,120,stopH},
        {0,routeBlue,180,stopI},
        {0,routeBlue,120,stopB},
        {0,walk,WalkSecsBA,stopA}
      ],
      fnInstQty = 40
    } when WalkSecsBA > 60*7 andalso WalkSecsBA < 60*8,
      optimal_trip_to_stop(
        #trip_config{tabs = Tabs, stopZid = stopA, totalSecsAllowed = 10000, totalTxfrAllowed = 10},
        [{0, walk, 0, stopZ}])).

optimal_trip_between_coords(Tabs, FromCoords, ToCoords, TotalTransfersAllowed) ->
  % Algorithm:
  % Let FromStops be the set of stops walkable from FromCoords.
  % Let ToStops be the set of stops walkable to ToCoords.
  % For every pair of stops in the Cartesian product of FromStops and ToStops,
  %   find the optimal route.
  % Return the time of the fastest optimal route.
  [{stops, StopsTab}] = ets:lookup(Tabs, stops),
  FromStops = stops_walkable_from_coords(StopsTab, FromCoords, ?MAX_WALK_SECS_TO_NEXT_STOP),
  ToStops = stops_walkable_from_coords(StopsTab, ToCoords, ?MAX_WALK_SECS_TO_NEXT_STOP),
  io:fwrite("optimal_trip_between_coords: FromStops=~w ToStops=~w~n", [FromStops, ToStops]),

  TripsPlusFinalWalks = [
    {
      optimal_trip_to_stop(
        #trip_config{
          tabs = Tabs,
          stopZid = ToStop#stop.id,
          totalTxfrAllowed = TotalTransfersAllowed
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
      optimalTrip = TripResult#trip_result.optimalTrip ++ [FinalWalk],
      fnInstQty = TripResult#trip_result.fnInstQty
    } end,
    TripsPlusFinalWalks),
%%  io:fwrite("OptimalTripResults: ~p~n", [OptimalTripResults]),
  min_by(OptimalTripResults, fun total_secs_of_trip_result/1).

trip_test_optimal_trip_between_coords(Tabs) ->
  FromCoords = #coords{lat=47, lon=-122}, % a little farther to stop B than stop A
  ToCoords = #coords{lat=47.032, lon=-122},
  TripResult = optimal_trip_between_coords(Tabs, FromCoords, ToCoords, 2),
  % from-stops: {A, B}.  to-stops: {E, F}.
  % walk to stop B, wait 5 minutes for yellow route, then take it to stop E.
  ?assertMatch(
    #trip_result{
      optimalTrip = [
        {0, walk, WalkSecsToB, stopB},
        {60*3, routeYellow, 60*3, stopC},
        {60*3, routeGreen, 60*3, stopF},
        {0, walk, WalkSecsFromF, to_dest_coords}
      ],
      fnInstQty = 28
    } when WalkSecsToB > 60*9 andalso WalkSecsToB < 60*10
      andalso WalkSecsFromF > 60*1 andalso WalkSecsFromF < 60*2, TripResult).

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
      fun trip_test_optimal_trip_to_stop_AZ_limitedTxfr/1,
      fun trip_test_optimal_trip_to_stop_ZA/1,
      fun trip_test_stops_walkable_from_stop/1,
      fun trip_test_optimal_trip_between_coords/1
    ]}
  }.
