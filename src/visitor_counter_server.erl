-module(visitor_counter_server).
-behavior(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, terminate/2]).

start() ->
  io:fwrite("Starting Visitor Counter~n"),
  gen_server:start_link({local, visitor_counter},
    visitor_counter_server, [], []).

init(_Args) ->
  {ok, 0}.

handle_call(newvisitor, _From, VisitorCount) ->
  NewVisitorCount = VisitorCount + 1,
  {reply, {visitors, NewVisitorCount}, NewVisitorCount}.

handle_cast(newvisitor, VisitorCount) ->
  NewVisitorCount = VisitorCount + 1,
  {noreply, NewVisitorCount}.

terminate(_Reason, _LoopData) ->
  ok.
