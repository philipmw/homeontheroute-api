-module(hello_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, nostate}.

handle(Req, _State) ->
  {visitors, VisitorCounter} = gen_server:call(visitor_counter, newvisitor),
  {ok, Req2} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"text/plain">>}],
    [<<"Hello from Erlang!  You are visitor # ">>,
      integer_to_binary(VisitorCounter)],
    Req),
  {ok, Req2, _State}.

terminate(_Reason, _Req, _State) ->
  ok.
