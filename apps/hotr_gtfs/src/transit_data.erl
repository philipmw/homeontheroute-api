-module(transit_data).

-export([
  create_all_ets/0,
  ets_map/2,
  stop/2,
  stops_list/1
]).

-include_lib("eunit/include/eunit.hrl").

-include("records/sconn.hrl").
-include("test_data.hrl").

create_all_ets() ->
  TopTableId = ets:new(transit_data_top, [
    named_table,
    {read_concurrency, true}]),
  ets:insert(TopTableId, {stops, create_stops_ets()}),
  ets:insert(TopTableId, {routes, create_routes_ets()}),
  ets:insert(TopTableId, {trips, create_trips_ets()}),
  ets:insert(TopTableId, {segments, create_segments_ets()}),
  ets:insert(TopTableId, {sconns, create_sconns_ets(TopTableId)}),
  ets:insert(TopTableId, {optimal_trips, create_optimal_trips_ets()}),
  TopTableId.

create_optimal_trips_ets() ->
  io:fwrite("Creating Optimal Trips table~n"),
  TableId = transit_optimal_trips_data:table_from_file(),
  TableId.

create_stops_ets() ->
  io:fwrite("Creating Stops table~n"),
  TableId = transit_stops_data:make_table(),
  Stops = transit_stops_data:load_from_file(gtfs:filename_for(stops)),
  io:fwrite("Read ~B stops from file~n", [lists:flatlength(Stops)]),
  ok = transit_stops_data:insert_to_table(Stops, TableId),
  io:fwrite("Loaded ~B stops into ETS table ID ~w~n", [ets:info(TableId, size), TableId]),
  TableId.

create_routes_ets() ->
  io:fwrite("Creating Routes table~n"),
  TableId = transit_routes_data:make_table(),
  Routes = transit_routes_data:load_from_file(gtfs:filename_for(routes)),
  io:fwrite("Read ~B routes from file~n", [lists:flatlength(Routes)]),
  ok = transit_routes_data:insert_to_table(Routes, TableId),
  io:fwrite("Loaded ~B routes into ETS table ID ~w~n", [ets:info(TableId, size), TableId]),
  TableId.

create_trips_ets() ->
  io:fwrite("Creating Trips table~n"),
  TableId = transit_trips_data:make_table(),
  Trips = transit_trips_data:load_from_file(gtfs:filename_for(trips)),
  io:fwrite("Read ~B trips from file~n", [lists:flatlength(Trips)]),
  ok = transit_trips_data:insert_to_table(Trips, TableId),
  io:fwrite("Loaded ~B trips into ETS table ID ~w~n", [ets:info(TableId, size), TableId]),
  TableId.

create_segments_ets() ->
  io:fwrite("Creating Segments table~n"),
  TableId = transit_segments_data:make_table(),
  Segments = transit_segments_data:load_from_file(gtfs:filename_for(stop_times)),
  io:fwrite("Assembled ~B transit segments~n", [lists:flatlength(Segments)]),
  ok = transit_segments_data:insert_to_table(TableId, Segments),
  io:fwrite("Loaded ~B transit segments into ETS table ID ~w~n", [ets:info(TableId, size), TableId]),
  TableId.

create_sconns_ets(TopTableId) ->
  io:fwrite("Creating Sconns table~n"),
  TableId = transit_sconns_data:make_table(),
  [{segments, SegTableId}] = ets:lookup(TopTableId, segments),
  [{routes, RouteTableId}] = ets:lookup(TopTableId, routes),
  [{trips, TripTableId}] = ets:lookup(TopTableId, trips),
  Sconns = transit_sconns_data:assemble(RouteTableId, SegTableId, TripTableId),
  io:fwrite("Assembled ~B sconns~n", [lists:flatlength(Sconns)]),
  ok = transit_sconns_data:insert_to_table(Sconns, TableId),
  io:fwrite("Loaded ~B sconns into ETS table ID ~w~n", [ets:info(TableId, size), TableId]),
  TableId.

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
  [{stops, StopsTab}] = ets:lookup(Tabs, stops),
  Mapped = ets_map(StopsTab, fun(_X) -> 1 end),
  ?assertEqual(10, lists:sum(Mapped)).

stops_list(StopsTab) -> ets:tab2list(StopsTab).

% Map stop IDs to stop records.  This assumes that the ETS table is a set!
stop(StopsTab, StopIds) ->
  lists:map(
    fun(StopId) -> lists:nth(1, ets:lookup(StopsTab, StopId)) end,
    StopIds
  ).


-spec sconns_between(ets:tid(), _, _) -> [sconn()].
sconns_between(SConnsTab, StopAId, StopBId) ->
  SConnsFromA = ets:lookup(SConnsTab, StopAId),
  lists:filter(fun (Sconn) -> Sconn#sconn.to_stop_id == StopBId end, SConnsFromA).

transit_data_test_sconns_between(Tabs) ->
  [{sconns, SConnsTab}] = ets:lookup(Tabs, sconns),
  ?assertEqual(
    [?TEST_SCONN_RED_A_B],
    sconns_between(SConnsTab, stopA, stopB)
  ).

%%%%%%%%%
% TESTING

transit_data_test_() ->
  {
    setup,
    fun test_data:setup_transit_data/0,
    fun test_data:teardown_transit_data/1,
    {with, [
      fun transit_data_test_ets_map/1,
      fun transit_data_test_sconns_between/1
    ]}
  }.

