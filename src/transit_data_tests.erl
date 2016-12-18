-module(transit_data_tests).

-include_lib("eunit/include/eunit.hrl").

-include("./records/stop.hrl").

load_stops_test() ->
  Stops = transit_data:load_stops_from_file("metro-gtfs-2016-11-09"),
  [Stop|_] = Stops,
  ?assertEqual(Stop, #stop{
    id = <<"1000">>,
    name = <<"Pine St & 9th Ave">>,
    lat = 47.6134148,
    lon = -122.332138}).

ejson_test() ->
  Stops = transit_data:load_stops_from_file("metro-gtfs-2016-11-09"),
  [Stop|_] = Stops,
  Ejson = transit_data:stop_to_ejson(Stop),
  ?assertEqual(Ejson, [
    {id, <<"1000">>},
    {name, <<"Pine St & 9th Ave">>},
    {lat, 47.6134148},
    {lon, -122.332138}
  ]).
