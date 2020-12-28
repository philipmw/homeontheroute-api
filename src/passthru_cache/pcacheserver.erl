-module(pcacheserver).

-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include("request.hrl").
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {
  tableId
}).

init({ets_new_opts, EtsNewOpts}) ->
  TableId = ets:new(cache_table, EtsNewOpts),
  {ok, #state{tableId = TableId}}.

handle_call(#cachereq{} = Request, _From, #state{} = State) ->
  DbResults = ets:lookup(State#state.tableId, Request#cachereq.key),
  FinalResult = case DbResults of
    [] ->
      MissFn = Request#cachereq.missFn,
      LiveResult = MissFn(Request#cachereq.key),
      ets:insert(State#state.tableId, LiveResult),
      LiveResult;
    [DbResult] ->
      DbResult
  end,
  {reply, FinalResult, State}.

handle_cast(_Request, _State) ->
  {error, notsupported}.

terminate(_Reason, State) ->
  ets:delete(State#state.tableId).

%%%%%
% Testing in the shell:
%
% rd(cachereq, { key, missFn }).
% rd(mydata, { k, v }).
% {ok, MyCache} = gen_server:start_link(pcacheserver, {ets_new_opts, [{keypos, #mydata.k}]}, []).
% gen_server:call(MyCache, #cachereq{key=35, missFn=fun(K) -> #mydata{k=K, v=K*2} end}).
% gen_server:call(MyCache, #cachereq{key=35, missFn=fun(K) -> #mydata{k=K, v=K*2} end}).

-record(teststruct, { k, v }).

server_test() ->
  {ok, Pid} = gen_server:start_link(pcacheserver, {ets_new_opts, [{keypos, #teststruct.k}]}, []),
  ?assertEqual(#teststruct{k=35, v=70},
    gen_server:call(Pid, #cachereq{key = 35, missFn = fun(K) -> #teststruct{k=K, v=K*2} end})),
  % We check that `missFn` isn't called by giving a function that gives a wrong result.
  ?assertEqual(#teststruct{k=35, v=70},
    gen_server:call(Pid, #cachereq{key = 35, missFn = fun(K) -> #teststruct{k=K, v=K*200} end})),
  gen_server:stop(Pid).
