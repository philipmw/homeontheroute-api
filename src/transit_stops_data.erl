-module(transit_stops_data).
-export([
  insert_to_table/2,
  load_from_file/1,
  make_table/0
]).

-include_lib("eunit/include/eunit.hrl").
-include("gtfs.hrl").
-include("records/coords.hrl").
-include("records/stop.hrl").

make_table() ->
  ets:new(stops, [{keypos, #stop.id}]).

load_from_file(GtfsBasedir) ->
  Filename = case application:get_application() of
               {ok, AppName} -> code:priv_dir(AppName) ++ "/" ++ GtfsBasedir ++ "/stops.txt";
               _ -> "./priv/" ++ GtfsBasedir ++ "/stops.txt"
             end,
  io:fwrite("Loading stops from ~s~n", [Filename]),
  {ok, DataBinary} = file:read_file(Filename),
  DataBinaryList = binary:split(DataBinary, <<$\n>>, [global]),
  [fileline_to_stop(B) || B <- select_good_lines(DataBinaryList)].

load_from_file_test() ->
  Stops = load_from_file(?GTFS_BASEDIR),
  [Stop|_] = Stops,
  ?assertEqual(Stop, #stop{
    id = <<"1000">>,
    name = <<"Pine St & 9th Ave">>,
    coords = #coords{
      lat = 47.6134148,
      lon = -122.332138
    }
  }).

insert_to_table([Stop|StopRest], StopsTableId) ->
  true = ets:insert(StopsTableId, Stop),
  insert_to_table(StopRest, StopsTableId);
insert_to_table([], _StopsTableId) -> ok.

select_good_lines(StopsDataBinaryList) ->
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

