-module(transitscore_handler).

-behaviour(cowboy_http_handler).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

init(Req, _State) ->
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
  Req2 = cowboy_req:reply(200, #{
    <<"Content-Type">> => <<"application/json">>,
    <<"Access-Control-Allow-Origin">> => <<"*">>
  }, jsone:encode(GeoJSON), Req),
  {ok, Req2, _State}.
