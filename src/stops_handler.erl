-module(stops_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, nostate}.

handle(Req, _State) ->
  ok = gen_server:cast(visitor_counter, newvisitor),
  {stops, Stops} = gen_server:call(transit_server, stops),
  {ok, Req2} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>}],
      jsone:encode(Stops),
    Req),
  {ok, Req2, _State}.

terminate(_Reason, _Req, _State) ->
  ok.
