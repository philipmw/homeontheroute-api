-module(parallel).
-export([parmap_all/2, parmap_firstN/3]).

-include_lib("eunit/include/eunit.hrl").

parmap_all(F, L) ->
  Parent = self(),
  [receive {Pid, Result} -> Result end ||
    Pid <- [spawn_link(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

parmap_all_test() ->
  ?assertEqual([10, 20, 30, 40, 50],
    parmap_all(fun(N) -> 10*N end, [1, 2, 3, 4, 5])).

parmap_firstN(N, F, L) ->
    Part1 = parmap_all(F, lists:sublist(L, N)),
    Part2 = lists:map(F, lists:sublist(L, N+1, length(L))),
    Part1 ++ Part2.

parmap_firstN_test() ->
  ?assertEqual([10, 20, 30, 40, 50],
    parmap_firstN(1, fun(N) -> 10*N end, [1, 2, 3, 4, 5])),
  ?assertEqual([10, 20, 30, 40, 50],
    parmap_firstN(2, fun(N) -> 10*N end, [1, 2, 3, 4, 5])),
  ?assertEqual([10, 20, 30, 40, 50],
    parmap_firstN(10, fun(N) -> 10*N end, [1, 2, 3, 4, 5])).
