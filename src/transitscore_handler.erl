-module(transitscore_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include_lib("eunit/include/eunit.hrl").

init(_Type, Req, _Opts) ->
  {ok, Req, nostate}.

handle(Req, _State) ->
  ok = gen_server:cast(visitor_counter, newvisitor),
  GeoJSON = #{
    <<"type">> => <<"FeatureCollection">>,
    <<"features">> => [
      #{
        <<"type">> => <<"Feature">>,
        <<"geometry">> => #{
          <<"type">> => <<"Point">>,
          <<"coordinates">> => [47.611427, -122.337454]
        },
        <<"properties">> => #{
          <<"transitScore">> => 88
        }
      }
    ]
  },
  {ok, Req2} = cowboy_req:reply(200,
    [
      {<<"Content-Type">>, <<"application/json">>},
      {<<"Access-Control-Allow-Origin">>, <<"*">>}
    ],
    jsone:encode(GeoJSON),
    Req),
  {ok, Req2, _State}.

terminate(_Reason, _Req, _State) ->
  ok.

