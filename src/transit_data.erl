-module(transit_data).

-export([
  create_all_ets/0,
  ets_map/2,
  stop/2
]).

-include_lib("eunit/include/eunit.hrl").
-include("gtfs.hrl").
-include("records/sconn.hrl").
-include("test_data.hrl").

create_all_ets() ->
  TopTableId = ets:new(transit_data_top, [named_table]),
  ets:insert(TopTableId, {stops, create_stops_ets(TopTableId)}),
  ets:insert(TopTableId, {routes, create_routes_ets(TopTableId)}),
  TopTableId.

create_stops_ets(_) ->
  io:fwrite("Creating Stops table~n"),
  TableId = ets:new(stops, [{keypos, #stop.id}]),
  Stops = transit_stops_data:load_from_file(?GTFS_BASEDIR),
  ok = transit_stops_data:insert_to_table(Stops, TableId),
  io:fwrite("Loaded ~B stops into ETS table ID ~w~n", [lists:flatlength(Stops), TableId]),
  TableId.

create_routes_ets(_) ->
  io:fwrite("Creating Routes table~n"),
  TableId = ets:new(routes, []),
  Routes = transit_routes_data:load_from_file(?GTFS_BASEDIR),
  ok = transit_routes_data:insert_to_table(Routes, TableId),
  io:fwrite("Loaded ~B routes into ETS table ID ~w~n", [lists:flatlength(Routes), TableId]),
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
  {stops, StopsTab} = lists:nth(1, ets:lookup(Tabs, stops)),
  Mapped = ets_map(StopsTab, fun(_X) -> 1 end),
  ?assertEqual(7, lists:sum(Mapped)).

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
  {sconns, SConnsTab} = lists:nth(1, ets:lookup(Tabs, sconns)),
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

