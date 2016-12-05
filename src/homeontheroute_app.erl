%%%-------------------------------------------------------------------
%% @doc homeontheroute public API
%% @end
%%%-------------------------------------------------------------------

-module(homeontheroute_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  visitor_counter:start(),

  Dispatch = cowboy_router:compile([
      {'_', [{"/", hello_handler, []}]}
  ]),
  {ok, _} = cowboy:start_http(my_http_listener, 100,
      [{port, 8080}],
      [{env, [{dispatch, Dispatch}]}]
  ),
  homeontheroute_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  visitor_counter:stop(),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
