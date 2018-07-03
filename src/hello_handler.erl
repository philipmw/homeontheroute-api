-module(hello_handler).

-behaviour(cowboy_http_handler).

-export([init/2]).

init(Req, _State) ->
  {visitors, VisitorCounter} = gen_server:call(visitor_counter, newvisitor),
  Req2 = cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain">>
  }, [
    <<"Hello from Erlang!  You are visitor # ">>,
      integer_to_binary(VisitorCounter), $., $\n],
    Req),
  {ok, Req2, _State}.
