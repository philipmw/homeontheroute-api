% This is a bridge between Cowboy, which does not appear to be written
% to be a supervised worker, and the `supervision` OTP concept/module.

-module(webserver).
-export([start/1, loop/0]).

start(TransitTableId) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", hello_handler, []},
      {"/stops", stops_handler, TransitTableId},
      {"/transitscore-pins", transitscore_handler, []}
    ]}
  ]),
  io:fwrite("Starting webserver~n"),
  {ok, _} = cowboy:start_clear(my_http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),
  {ok, spawn_link(?MODULE, loop, [])}.

loop() ->
  process_flag(trap_exit, true),
  receive {'EXIT', _Pid, _Reason} ->
    io:fwrite("Stopping webserver~n"),
    cowboy:stop_listener(my_http_listener)
  end.