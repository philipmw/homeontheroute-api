-module(transit_server).
-behavior(gen_server).

-export([start/0, init/1, handle_call/3, terminate/2]).

-record(transitdata, {stops}).
-record(stop, {id, name, lat, lon}).

start() ->
  io:fwrite("Starting Transit Server~n"),
  gen_server:start_link({local, transit_server},
    transit_server,
    ["metro-gtfs-2016-11-09"], % args
    []). % opts

init(GtfsBasedir) ->
  {ok, AppName} = application:get_application(),
  StopsFilename = code:priv_dir(AppName) ++ "/" ++ GtfsBasedir ++ "/stops.txt",
  io:fwrite("Loading stops from ~s~n", [StopsFilename]),
  {ok, StopsDataBinary} = file:read_file(StopsFilename),
  StopsDataBinaryList = binary:split(StopsDataBinary, <<$\n>>, [global]),
  Stops = [fileline_to_stop(B) || B <- select_stop_lines(StopsDataBinaryList)],
  {ok, #transitdata{stops=Stops}}.

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
    {[{id, StopRec#stop.id}]},
    {[{name, StopRec#stop.name}]},
    {[{lat, StopRec#stop.lat}]},
    {[{lon, StopRec#stop.lon}]}
  ].

handle_call(stops, _From, TransitData) ->
  {reply, {
    stops,
    [stop_to_ejson(S) || S <- TransitData#transitdata.stops]
  }, TransitData}.

terminate(_Reason, _LoopData) ->
  io:fwrite("Stopping Transit Server~n"),
  ok.
