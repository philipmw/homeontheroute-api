-module(log).

-export([num_records/1, unique_queries/1]).

num_records(Log) ->
  num_records(Log, start, 0).

num_records(Log, Cont0, Qty) ->
  case disk_log:chunk(Log, Cont0) of
    eof -> Qty;
    {Cont1, Rows} -> num_records(Log, Cont1, Qty + length(Rows))
  end.

key_of_log_entry({
  optimal_trip_to_stop,
  invoked,
  {transitModeToA, TMToA},
  {stopAID, StopAId},
  {stopZID, StopZId},
  {walkedSecs, _},
  {stopsVisited, _},
  {transfersQty, _}
}) -> {TMToA, StopAId, StopZId}.

unique_queries(Log) ->
  unique_queries(Log, start, sets:new()).
unique_queries(Log, Cont0, QueriesAcc0) ->
  case disk_log:chunk(Log, Cont0) of
    eof -> QueriesAcc0;
    {Cont1, Rows} ->
      QueriesAcc = lists:foldl(fun(Row, QA00) -> sets:add_element(key_of_log_entry(Row), QA00) end, QueriesAcc0, Rows),
      unique_queries(Log, Cont1, QueriesAcc)
  end.
