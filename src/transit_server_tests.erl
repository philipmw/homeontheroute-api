-module(transit_server_tests).

-include_lib("eunit/include/eunit.hrl").

-include("./records/stop.hrl").
-include("./records/transitdata.hrl").

loopdata_test() ->
  ?debugMsg("Running loopdata test"),
  {ok, TransitData} = transit_server:init("metro-gtfs-2016-11-09"),
  Stops = TransitData#transitdata.stops,
  [Stop|_] = Stops,
  ?assertEqual(Stop, #stop{
    id = <<"1000">>,
    name = <<"Pine St & 9th Ave">>,
    lat = 47.6134148,
    lon = -122.332138}).

ejson_test() ->
  ?debugMsg("running ejson test"),
  {ok, TransitData} = transit_server:init("metro-gtfs-2016-11-09"),
  Stops = TransitData#transitdata.stops,
  [Stop|_] = Stops,
  Ejson = transit_server:stop_to_ejson(Stop),
  ?assertEqual(Ejson, [
    {id, <<"1000">>},
    {name, <<"Pine St & 9th Ave">>},
    {lat, 47.6134148},
    {lon, -122.332138}
  ]).
