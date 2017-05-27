-module(transit_data).

-export([create_all_ets/0]).

-include_lib("eunit/include/eunit.hrl").
-include("records/stop.hrl").
-include("records/sconn.hrl").
-include("test_data.hrl").
-include("earth.hrl").

% How much time is a typical person willing to spend walking?
-define(MAX_WALK_MINS, 10).

-define(GTFS_BASEDIR, "metro-gtfs").

create_all_ets() ->
  create_stops_ets().

create_stops_ets() ->
  io:fwrite("Creating Stops Table~n"),
  StopsTableId = ets:new(transit_stops, [set, named_table, {keypos, #stop.id}]),
  Stops = load_stops_from_file(?GTFS_BASEDIR),
  ok = insert_stops_to_table(Stops, StopsTableId),
  io:fwrite("Ok, loaded ~B stops into ETS~n", [lists:flatlength(Stops)]).

load_stops_from_file(GtfsBasedir) ->
  StopsFilename = case application:get_application() of
                    {ok, AppName} ->
                      code:priv_dir(AppName) ++ "/" ++ GtfsBasedir ++ "/stops.txt";
                    _ ->
                      "./priv/" ++ GtfsBasedir ++ "/stops.txt"
                  end,
  io:fwrite("Loading stops from ~s~n", [StopsFilename]),
  {ok, StopsDataBinary} = file:read_file(StopsFilename),
  StopsDataBinaryList = binary:split(StopsDataBinary, <<$\n>>, [global]),
  [fileline_to_stop(B) || B <- select_stop_lines(StopsDataBinaryList)].

load_stops_from_file_test() ->
  Stops = load_stops_from_file(?GTFS_BASEDIR),
  [Stop|_] = Stops,
  ?assertEqual(Stop, #stop{
    id = <<"1000">>,
    name = <<"Pine St & 9th Ave">>,
    lat = 47.6134148,
    lon = -122.332138}).

insert_stops_to_table([Stop|StopRest], StopsTableId) ->
  true = ets:insert(StopsTableId, Stop),
  insert_stops_to_table(StopRest, StopsTableId);
insert_stops_to_table([], _StopsTableId) -> ok.

select_stop_lines(StopsDataBinaryList) ->
  % skip the header
  HeaderlessList = lists:nthtail(1, StopsDataBinaryList),
  % skip empty lines
  lists:filter(fun(BL) -> BL /= <<>> end, HeaderlessList).

fileline_to_stop(BinaryLine) ->
%%  io:fwrite("Line: ~s~n", [BinaryLine]),
  Fields = binary:split(BinaryLine, <<$,>>, [global]),
%%  io:fwrite("Fields: ~s~n", [Fields]),
  #stop{
    id=lists:nth(1, Fields),
    name=binary:replace(lists:nth(3, Fields), <<$">>, <<>>, [global]),
    lat=binary_to_float(lists:nth(5, Fields)),
    lon=binary_to_float(lists:nth(6, Fields))
  }.

% `ets_map` passes each item from the ETS table to a user-specified function.
ets_map(Tab, Fun) ->
  ets_map(Tab, Fun, [], ets:first(Tab)).
ets_map(_Tab, _Fun, Mapped, '$end_of_table') -> Mapped;
ets_map(Tab, Fun, Mapped, Key) ->
  ets_map(Tab, Fun, Mapped, Key, ets:lookup(Tab, Key)).
ets_map(Tab, Fun, Mapped, Key, [Value]) ->
  NewMapped = [Fun(Value) | Mapped],
  ets_map(Tab, Fun, NewMapped, ets:next(Tab, Key)).

transit_data_test_ets_map(Tabs) ->
  {stops, StopsTab} = lists:nth(1, ets:lookup(Tabs, stops)),
  Mapped = ets_map(StopsTab, fun(_X) -> 1 end),
  ?assertEqual(5, lists:sum(Mapped)).

% Map stop IDs to stop records.  This assumes that the ETS table is a set!
stop(StopsTab, StopIds) ->
  lists:map(
    fun(StopId) -> lists:nth(1, ets:lookup(StopsTab, StopId)) end,
    StopIds
  ).

% All stops surrounding the input stop, excluding the given set of stops.
% Returns a list in the format:
%   [ {StopAId, MetersDistance}, {StopBId, MetersDistance}, ... ]
stops_walkable_from(StopsTab, FromStopId, StopIdsExcluded) ->
  [FromStop] = stop(StopsTab, [FromStopId]),
  Distances = ets_map(StopsTab, fun(StopX) ->
    {StopX, earth:meters_between_stops(FromStop, StopX)} end),
  lists:filter(
    fun({StopX, MetersAway}) ->
      (not lists:member(StopX#stop.id, StopIdsExcluded)) and
      (MetersAway < ?MAX_WALK_MINS*?WALK_METERS_PER_MIN)
    end,
    Distances).

transit_data_test_stops_walkable_from(Tabs) ->
  {stops, StopsTab} = lists:nth(1, ets:lookup(Tabs, stops)),
  ?assertEqual(
    [{?TEST_STOP_B, 216.28796299803383}],
    stops_walkable_from(StopsTab, stopA, [stopA])
  ).

-spec sconns_between(ets:tid(), _, _) -> [sconn()].
sconns_between(SConnsTab, StopAId, StopBId) ->
  SConnsFromA = ets:lookup(SConnsTab, StopAId),
  lists:filter(fun (Sconn) -> Sconn#sconn.to_stop_id == StopBId end, SConnsFromA).

transit_data_test_sconns_between(Tabs) ->
  {sconns, SConnsTab} = lists:nth(1, ets:lookup(Tabs, sconns)),
  ?assertEqual(
    [?TEST_SCONN_RED_A_B],
    sconns_between(SConnsTab, stopA, stopB)
  ).

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

connections_from_stop(SConnsTab, StopAId, StopIdsToExclude) ->
  AllSConns = ets:lookup(SConnsTab, StopAId),
  lists:filter(
    fun (SConn) -> not lists:member(SConn#sconn.to_stop_id, StopIdsToExclude) end,
    AllSConns).

transit_data_test_connections_from_stop_1(Tabs) ->
  {sconns, SConnsTab} = lists:nth(1, ets:lookup(Tabs, sconns)),
  ?assertEqual(
    [?TEST_SCONN_RED_B_C, ?TEST_SCONN_YELLOW_B_C],
    connections_from_stop(SConnsTab, stopB, [])
  ).

transit_data_test_connections_from_stop_2(Tabs) ->
  {sconns, SConnsTab} = lists:nth(1, ets:lookup(Tabs, sconns)),
  ?assertEqual(
    [?TEST_SCONN_GREEN_C_E],
    connections_from_stop(SConnsTab, stopC, [stopD])
  ).

% Find route between Stop A and Stop Z, when you arrived at Stop A
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
optimal_route(_Tabs, _StopsVisited, _TransitModeToA, StopId, StopId) -> [];
optimal_route(Tabs, StopsVisited, TransitModeToA, StopAId, StopZId) ->
  io:fwrite("optimal_route(~w, ~w, ~w, ~w)~n", [StopsVisited, TransitModeToA, StopAId, StopZId]),
  {stops, StopsTab} = lists:nth(1, ets:lookup(Tabs, stops)),
  {sconns, SConnsTab} = lists:nth(1, ets:lookup(Tabs, sconns)),

  % We are assembling this list:
  % [
  %   [{<mode-switch-mins>, <transit-mode>, <transit-time>, <to-stop-X>}, [...optimal-route-from-X]]
  %   [{3 min, Route 40, 2 min, Stop40040}, [...optimal route from Stop40040]],
  %   [{5 min, Route 28, 3 min, Stop28028}, [...optimal route from Stop28028]],
  % ]

  RoutesRidingThruStopB = [
    [{mins_of_mode_switch(StopAId, TransitModeToA, SConn#sconn.transit_mode, SConn#sconn.wait_mins),
      SConn#sconn.transit_mode,
      SConn#sconn.travel_mins,
      SConn#sconn.to_stop_id} |
      optimal_route(Tabs, [StopAId] ++ StopsVisited, SConn#sconn.transit_mode, SConn#sconn.to_stop_id, StopZId)] ||
    SConn <- connections_from_stop(SConnsTab, StopAId, [StopAId] ++ StopsVisited)
  ],
  io:fwrite("RoutesRidingThruStopB: ~w~n", [RoutesRidingThruStopB]),

  RoutesWalkingToB = [
    [{mins_of_mode_switch(StopAId, TransitModeToA, walk, 0),
      walk,
      DistanceAB / ?WALK_METERS_PER_MIN, % travel mins
      StopB#stop.id} |
      optimal_route(Tabs, [StopAId] ++ StopsVisited, walk, StopB#stop.id, StopZId)] ||
    {StopB, DistanceAB} <- stops_walkable_from(StopsTab, StopAId, [StopAId] ++ StopsVisited)
  ],
  io:fwrite("RoutesWalkingToB: ~w~n", [RoutesWalkingToB]),

  % Direct walking (as the crow flies) is an option too...
  [StopA, StopZ] = stop(StopsTab, [StopAId, StopZId]),
  DirectRoute = [[{0, walk, earth:direct_walk_mins(StopA, StopZ), StopZId}]],

  AllPossibleRoutes =
    RoutesRidingThruStopB ++
    RoutesWalkingToB ++
    DirectRoute,
  io:fwrite("AllPossibleRoutes: ~w~n", [AllPossibleRoutes]),
  min_by(AllPossibleRoutes, fun total_mins/1).

transit_data_test_optimal_route(Tabs) ->
  ?assertMatch(
    [
      {0, walk, WalkTime, stopB},
      {15, routeYellow, 5, stopC},
      {5, routeGreen, 5, stopE}
    ] when WalkTime > 2, % FIXME: https://stackoverflow.com/questions/44212785/multiple-clauses-in-eunits-assertmatch
    optimal_route(Tabs, [], walk, stopA, stopE)
  ).

%%%%%%%%%
% TESTING

transit_data_test_() ->
  {
    setup,
    fun setup_transit_data/0,
    fun teardown_transit_data/1,
    {with, [
      fun transit_data_test_ets_map/1,
      fun transit_data_test_sconns_between/1,
      fun transit_data_test_connections_from_stop_1/1,
      fun transit_data_test_connections_from_stop_2/1,
      fun transit_data_test_optimal_route/1,
      fun transit_data_test_stops_walkable_from/1
    ]}
  }.

setup_transit_data() ->
  TablesTableId = ets:new(transit_data_unit_tables, [set]),

  StopsTableId = ets:new(transit_data_unit_stops, [set, {keypos, #stop.id}]),
  ets:insert(TablesTableId, {stops, StopsTableId}),
  ets:insert(StopsTableId, ?TEST_STOP_A),
  ets:insert(StopsTableId, ?TEST_STOP_B),
  ets:insert(StopsTableId, ?TEST_STOP_C),
  ets:insert(StopsTableId, ?TEST_STOP_D),
  ets:insert(StopsTableId, ?TEST_STOP_E),
  io:fwrite("Inserted test stops data into ~w~n", [StopsTableId]),

  SConnsTableId = ets:new(transit_data_unit_sconns, [bag, {keypos, #sconn.from_stop_id}]),
  ets:insert(TablesTableId, {sconns, SConnsTableId}),
  ets:insert(SConnsTableId, ?TEST_SCONN_RED_A_B),
  ets:insert(SConnsTableId, ?TEST_SCONN_RED_B_C),
  ets:insert(SConnsTableId, ?TEST_SCONN_RED_C_D),
  ets:insert(SConnsTableId, ?TEST_SCONN_RED_D_E),
  ets:insert(SConnsTableId, ?TEST_SCONN_YELLOW_B_C),
  ets:insert(SConnsTableId, ?TEST_SCONN_YELLOW_C_D),
  ets:insert(SConnsTableId, ?TEST_SCONN_YELLOW_D_E),
  ets:insert(SConnsTableId, ?TEST_SCONN_GREEN_C_E),
  io:fwrite("Inserted test stops connections data into ~w~n", [SConnsTableId]),

  TablesTableId.

teardown_transit_data(TablesTableId) ->
  ets:foldl(fun ({_, TableId}, _) -> ets:delete(TableId) end, acc, TablesTableId).
