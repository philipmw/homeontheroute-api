-module(stops_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, nostate}.


handle(Req, _State) ->
  ok = gen_server:cast(visitor_counter, newvisitor),
  Stops = ets:foldl(fun (E, A) -> A ++ [transit_data:stop_to_ejson(E)] end, [], transit_stops),
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
