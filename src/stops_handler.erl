-module(stops_handler).

-behaviour(cowboy_http_handler).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("./records/coords.hrl").
-include("./records/stop.hrl").

init(Req, _State) ->
  ok = gen_server:cast(visitor_counter, newvisitor),
  Stops = ets:foldl(fun (E, A) -> A ++ [stop_to_ejson(E)] end, [], transit_stops),
  Req2 = cowboy_req:reply(200, #{
    <<"Content-Type">> => <<"application/json">>,
    <<"Access-Control-Allow-Origin">> => <<"*">>
  }, jsone:encode(Stops),
    Req),
  {ok, Req2, _State}.

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
