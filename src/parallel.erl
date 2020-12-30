-module(parallel).
-export([parmap_all/2, parmap_first/2]).

-include_lib("eunit/include/eunit.hrl").

parmap_all(F, L) ->
  Parent = self(),
  [receive {Pid, Result} -> Result end ||
    Pid <- [spawn_link(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

parmap_all_test() ->
  ?assertEqual([10, 20, 30, 40, 50],
    parmap_all(fun(N) -> 10*N end, [1, 2, 3, 4, 5])).

parmap_first(F, L) ->
  % To avoid too many processes, spawn the first element into its own process,
  % and process the rest locally.
  if length(L) > 1 ->
    [FirstElem|RestElems] = L,
    Parent = self(),
    Pid = spawn_link(fun() -> Parent ! {self(), F(FirstElem)} end),

    % meanwhile, map the rest
    ResultRest = lists:map(F, RestElems),

    ResultFirst = receive {Pid, Result} -> Result end,
    [ResultFirst] ++ ResultRest; % inefficient, but ok
    true -> % 0 or 1 elements, just use map()
      lists:map(F, L) end.

parmap_first_test() ->
  ?assertEqual([10, 20, 30, 40, 50],
    parmap_first(fun(N) -> 10*N end, [1, 2, 3, 4, 5])).
