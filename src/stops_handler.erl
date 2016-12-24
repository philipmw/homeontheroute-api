-module(stops_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include_lib("eunit/include/eunit.hrl").
-include("./records/stop.hrl").

init(_Type, Req, _Opts) ->
  {ok, Req, nostate}.

handle(Req, _State) ->
  ok = gen_server:cast(visitor_counter, newvisitor),
  Stops = ets:foldl(fun (E, A) -> A ++ [stop_to_ejson(E)] end, [], transit_stops),
  {ok, Req2} = cowboy_req:reply(200,
    [
      {<<"Content-Type">>, <<"application/json">>},
      {<<"Access-Control-Allow-Origin">>, <<"http://homeontheroute.com">>}
    ],
      jsone:encode(Stops),
    Req),
  {ok, Req2, _State}.

terminate(_Reason, _Req, _State) ->
  ok.

stop_to_ejson(StopRec) ->
  [
    {id, StopRec#stop.id},
    {name, StopRec#stop.name},
    {lat, StopRec#stop.lat},
    {lon, StopRec#stop.lon}
  ].

stop_to_ejson_test() ->
  Stop = #stop{id = <<"1000">>,
    name = <<"Pine St & 9th Ave">>,
    lat = 47.6134148,
    lon = -122.332138},
  Ejson = stop_to_ejson(Stop),
  ?assertEqual(Ejson, [
    {id, <<"1000">>},
    {name, <<"Pine St & 9th Ave">>},
    {lat, 47.6134148},
    {lon, -122.332138}
  ]).
