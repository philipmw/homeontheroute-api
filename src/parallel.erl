-module(parallel).
-export([parmap/2]).

-include_lib("eunit/include/eunit.hrl").

parmap(F, L) ->
  Parent = self(),
  [receive {Pid, Result} -> Result end ||
    Pid <- [spawn_link(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

parmap_test() ->
  ?assertEqual([10, 20, 30, 40, 50],
    parmap(fun(N) -> 10*N end, [1, 2, 3, 4, 5])).