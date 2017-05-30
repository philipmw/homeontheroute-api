-module(test_data).
-export([
  setup_transit_data/0,
  teardown_transit_data/1
]).

-include("test_data.hrl").

setup_transit_data() ->
  TablesTableId = ets:new(transit_data_unit_tables, [set]),

  StopsTableId = ets:new(transit_data_unit_stops, [set, {keypos, #stop.id}]),
  ets:insert(TablesTableId, {stops, StopsTableId}),
  ets:insert(StopsTableId, ?TEST_STOP_A),
  ets:insert(StopsTableId, ?TEST_STOP_B),
  ets:insert(StopsTableId, ?TEST_STOP_C),
  ets:insert(StopsTableId, ?TEST_STOP_D),
  ets:insert(StopsTableId, ?TEST_STOP_E),
  io:fwrite("Inserted test stops data into ~w~n", [StopsTableId]),

  SConnsTableId = ets:new(transit_data_unit_sconns, [bag, {keypos, #sconn.from_stop_id}]),
  ets:insert(TablesTableId, {sconns, SConnsTableId}),
  ets:insert(SConnsTableId, ?TEST_SCONN_RED_A_B),
  ets:insert(SConnsTableId, ?TEST_SCONN_RED_B_C),
  ets:insert(SConnsTableId, ?TEST_SCONN_RED_C_D),
  ets:insert(SConnsTableId, ?TEST_SCONN_RED_D_E),
  ets:insert(SConnsTableId, ?TEST_SCONN_YELLOW_B_C),
  ets:insert(SConnsTableId, ?TEST_SCONN_YELLOW_C_D),
  ets:insert(SConnsTableId, ?TEST_SCONN_YELLOW_D_E),
  ets:insert(SConnsTableId, ?TEST_SCONN_GREEN_C_E),
  io:fwrite("Inserted test stops connections data into ~w~n", [SConnsTableId]),

  TablesTableId.

teardown_transit_data(TablesTableId) ->
  ets:foldl(fun ({_, TableId}, _) -> ets:delete(TableId) end, acc, TablesTableId).
