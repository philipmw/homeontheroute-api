-module(transit_optimal_trips_data).

-export([
  table_from_data/1,
  table_from_file/0
]).

-include("records/optimal_trip.hrl").
-include("records/stop.hrl").
-include("records/trip.hrl").

-ifndef(FILENAME).
-define(FILENAME, "./apps/hotr_gtfs/priv/optimal_trips.ets").
-endif.

table_from_file() ->
  {ok, Tab} = ets:file2tab(?FILENAME, [{verify, true}]),
  io:fwrite("Loaded ~B optimal trips~n", [ets:info(Tab, size)]),
  Tab.

table_from_data(Tabs) ->
  OptiTripsTab = ets:new(optimal_trips, [
    {write_concurrency, true},
    {keypos, #optimal_trip.trip_ends}]),

  [{stops, StopsTab}] = ets:lookup(Tabs, stops),
  for_each_pair_of_stops(StopsTab, fun (StopA, StopZ) ->
    TripEnds = #trip_ends{
      from_stop_id = StopA#stop.id,
      to_stop_id = StopZ#stop.id
    },
    {ComputeMicrosecs, TripResult} = timer:tc(trip, optimal_trip_to_stop, [
      #trip_config{
        tabs = Tabs,
        stopZid = StopZ#stop.id,
        totalTransfersAllowed = 3
      },
      #trip_state{
        remainAllowedSecs = 60 * 90,
        segs = [{0, walk, 0, StopA#stop.id}]
      }
    ]),
    OptimalTrip = #optimal_trip{
      trip_ends = TripEnds,
      trip_result = TripResult,
      trip_secs = trip:total_secs_of_trip_result(TripResult),
      compute_secs = ComputeMicrosecs / 1000 / 1000
    },
    io:fwrite("~p~n", [OptimalTrip]),
    true = ets:insert_new(OptiTripsTab, OptimalTrip)
    end),
  io:fwrite("Finished with ~B optimal trips generated~n", [ets:info(OptiTripsTab, size)]),
  ok = ets:tab2file(OptiTripsTab, ?FILENAME, [{extended_info, [md5sum, object_count]}]),
  OptiTripsTab.

for_each_pair_of_stops(StopsTab, Fn) ->
  StopsList = transit_data:stops_list(StopsTab),
  StopsOrdDict = array:to_orddict(array:from_list(StopsList)),
  StopsSize = orddict:size(StopsList),
  orddict:map(fun (IdxA, StopA) ->
    orddict:map(fun (IdxZ, StopZ) ->
      PctComplete = 100 * (IdxA * StopsSize + IdxZ) / (StopsSize * StopsSize),
      io:fwrite("stopA: ~B/~B, stopB: ~B/~B (~f%):~n",
        [IdxA, StopsSize, IdxZ, StopsSize, PctComplete]),
      Res = Fn(StopA, StopZ),
      Res
              end, StopsOrdDict)
            end, StopsOrdDict).
