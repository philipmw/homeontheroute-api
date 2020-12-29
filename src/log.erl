-module(log).

-export([
  log_to_file/2,
  num_records/1,
  unique_queries/1]).

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
  {initSegs, _},
  {transitModeToA, TMToA},
  {stopAID, StopAId},
  {stopZID, StopZId},
  {totalTripSecs, _},
  {walkedSecs, _},
  {stopIdsVisited, _},
  {transfersQty, _},
  {canContinueTrip, _},
  {canTransfer, _}
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

log_to_file(Log, IoDevice) ->
  log_to_file(Log, start, IoDevice).
log_to_file(Log, Cont0, IoDevice) ->
  case disk_log:chunk(Log, Cont0) of
    eof -> ok;
    {Cont1, Rows} ->
      lists:map(fun(Row) -> io:fwrite(IoDevice, "~p~n", [Row]) end, Rows),
      log_to_file(Log, Cont1, IoDevice)
  end.
