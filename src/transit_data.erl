-module(transit_data).

-export([
  create_all_ets/0,
  ets_map/2,
  stop/2
]).

-include_lib("eunit/include/eunit.hrl").
-include("records/stop.hrl").
-include("records/sconn.hrl").
-include("earth.hrl").
-include("person.hrl").
-include("test_data.hrl").

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
    coords = #coords{
      lat = 47.6134148,
      lon = -122.332138
    }
  }).

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
    coords=#coords{
      lat=binary_to_float(lists:nth(5, Fields)),
      lon=binary_to_float(lists:nth(6, Fields))
    }
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
  ?assertEqual(6, lists:sum(Mapped)).

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

