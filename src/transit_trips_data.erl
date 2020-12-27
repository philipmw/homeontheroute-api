-module(transit_trips_data).
-export([
  insert_to_table/2,
  load_from_file/1,
  make_table/0
]).

-include_lib("eunit/include/eunit.hrl").
-include("gtfs.hrl").
-include("records/trip.hrl").

make_table() ->
  ets:new(trips, [{keypos, #trip.trip_id}]).

load_from_file(GtfsBasedir) ->
  Filename = case application:get_application() of
               {ok, AppName} -> code:priv_dir(AppName) ++ "/" ++ GtfsBasedir ++ "/trips.txt";
               _ -> "./priv/" ++ GtfsBasedir ++ "/trips.txt"
             end,
  io:fwrite("Loading trips from ~s~n", [Filename]),
  {ok, DataBinary} = file:read_file(Filename),
  DataBinaryList = binary:split(DataBinary, <<$\n>>, [global]),
  [fileline_to_trip(B) || B <- select_good_lines(DataBinaryList)].

load_from_file_test() ->
  Trips = load_from_file(?GTFS_BASEDIR),
  [Trip|_] = Trips,
  ?assertEqual(Trip, #trip{
    trip_id = <<"34745815">>,
    route_id = <<"100160">>
  }).

insert_to_table([Route|Rest], RoutesTableId) ->
  true = ets:insert(RoutesTableId, Route),
  insert_to_table(Rest, RoutesTableId);
insert_to_table([], _) -> ok.

select_good_lines(DataBinaryList) ->
  % skip the header
  HeaderlessList = lists:nthtail(1, DataBinaryList),
  % skip empty lines
  lists:filter(fun(BL) -> BL /= <<>> end, HeaderlessList).

fileline_to_trip(BinaryLine) ->
%%  io:fwrite("Line: ~s~n", [BinaryLine]),
  Fields = binary:split(BinaryLine, <<$,>>, [global]),
%%  io:fwrite("Fields: ~s~n", [Fields]),
  #trip{
    trip_id = lists:nth(3, Fields),
    route_id = lists:nth(1, Fields)
  }.

