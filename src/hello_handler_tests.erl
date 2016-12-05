-module(hello_handler_tests).

-include_lib("eunit/include/eunit.hrl").

basic_response_test_() ->
  ?debugMsg("about to do test setup"),
  {setup,
    fun start_visitor_counter/0,
    fun stop_visitor_counter/1,
    [fun basic_response_test_real/0]}.

start_visitor_counter() ->
  ?debugMsg("starting visitor counter in tests"),
  {ok, Pid} = visitor_counter:start(),
  Pid.

stop_visitor_counter(Pid) ->
  ?debugMsg("stopping visitor counter in tests"),
  gen_server:stop(Pid).

basic_response_test_real() ->
  ?debugMsg("running test"),
  meck:new(cowboy_req),
  meck:expect(cowboy_req, reply, fun(200, _Headers, Body, _Req) -> {ok, Body} end),
  {ok, Response, _State} = hello_handler:handle({}, nostate),
  ?assert(meck:validate(cowboy_req)),
  meck:unload(cowboy_req),
  ?debugMsg("Response is: " ++ Response),
  ?assertEqual("Hello from Erlang!  You are visitor # 1",
    binary_to_list(iolist_to_binary(Response))).
