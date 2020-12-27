-module(stops_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("./records/coords.hrl").
-include("./records/stop.hrl").

init(Req, TransitTableId) ->
  ok = gen_server:cast(visitor_counter, newvisitor),
  {stops, StopsTableId} = lists:nth(1, ets:lookup(TransitTableId, stops)),
  Stops = ets:foldl(fun (E, A) -> A ++ [stop_to_ejson(E)] end, [], StopsTableId),
  Req2 = cowboy_req:reply(200, #{
    <<"Content-Type">> => <<"application/json">>,
    <<"Access-Control-Allow-Origin">> => <<"*">>
  }, jsone:encode(Stops),
    Req),
  {ok, Req2, TransitTableId}.

stop_to_ejson(StopRec) ->
  [
    {id, StopRec#stop.id},
    {name, StopRec#stop.name},
    {lat, (StopRec#stop.coords)#coords.lat},
    {lon, (StopRec#stop.coords)#coords.lon}
  ].

stop_to_ejson_test() ->
  Stop = #stop{id = <<"1000">>,
    name = <<"Pine St & 9th Ave">>,
    coords = #coords{
      lat = 47.6134148,
      lon = -122.332138
    }
  },
  Ejson = stop_to_ejson(Stop),
  ?assertEqual(Ejson, [
    {id, <<"1000">>},
    {name, <<"Pine St & 9th Ave">>},
    {lat, 47.6134148},
    {lon, -122.332138}
  ]).
