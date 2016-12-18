-module(transit_data).

-export([create_stops_ets/0,
  load_stops_from_file/1,
  insert_stops_to_table/2,
  stop_to_ejson/1]).

-include("./records/stop.hrl").

create_stops_ets() ->
  io:fwrite("Creating Stops Table~n"),
  StopsTableId = ets:new(transit_stops, [set, named_table, {keypos, #stop.id}]),
  Stops = load_stops_from_file("metro-gtfs-2016-11-09"),
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

stop_to_ejson(StopRec) ->
  [
    {id, StopRec#stop.id},
    {name, StopRec#stop.name},
    {lat, StopRec#stop.lat},
    {lon, StopRec#stop.lon}
  ].
