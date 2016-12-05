-module(visitor_counter).
-behavior(gen_server).

-export([start/0, init/1, handle_call/3, terminate/2]).

start() ->
  io:fwrite("Starting Visitor Counter~n"),
  gen_server:start_link({local, visitor_counter},
    visitor_counter, [], []).

init(_Args) ->
  {ok, 0}.

handle_call(newvisitor, _From, VisitorCount) ->
  io:fwrite("Visitor Counter received a new visit~n"),
  NewVisitorCount = VisitorCount + 1,
  {reply, {visitors, NewVisitorCount}, NewVisitorCount}.

terminate(_Reason, _LoopData) ->
  io:fwrite("Visitor Counter is shutting down~n").
