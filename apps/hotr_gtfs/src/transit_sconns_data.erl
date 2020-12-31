-module(transit_sconns_data).
-export([
  assemble/3,
  insert_to_table/2,
  make_table/0
]).

-include_lib("eunit/include/eunit.hrl").
-include("gtfs.hrl").
-include("records/route.hrl").
-include("records/segment.hrl").
-include("records/sconn.hrl").
-include("records/trip.hrl").

make_table() ->
  ets:new(sconns, [bag, {keypos, #sconn.from_stop_id}]).

assemble(RouteTableId, SegmentTableId, TripTableId) ->
  Segments = ets:select(SegmentTableId, [{'_', [], ['$_']}]),
  % We have many segments for a given (route, from-stop, to-stop), for example
  % 210 segments for route 62 traveling between 18540 and 18550.
  % What differs is arrival, departure, and travel time.
  % We need to aggregate them into a single Sconn.

  % Create a map from the (route, from-stop, to-stop) tuple to the segments,
  % so we can easily aggregate the values.
  SegmentsMap = lists:foldl(
    fun(Segment, SegmentsMap) ->
      Route = route_of_trip_id(RouteTableId, TripTableId, Segment#segment.trip_id),
      maps:update_with(
        {Route, Segment#segment.from_stop_id, Segment#segment.to_stop_id},
        fun(SegsForKey) -> [Segment] ++ SegsForKey end,
        [Segment],
        SegmentsMap)
    end,
    maps:new(),
    Segments),

  % Aggregate
  maps:fold(
    fun({Route, FromStopId, ToStopId} = _Key, SegsForKey, SconnsAcc) ->
      AvgWaitSecs = 24 * 60 * 60 / length(SegsForKey),
      AvgTravelSecs = lists:sum(lists:map(fun(Seg) -> Seg#segment.travel_secs end, SegsForKey)) / length(SegsForKey),
      [#sconn{
        from_stop_id = FromStopId,
        to_stop_id = ToStopId,
        transit_mode = Route#route.short_name,
        wait_secs = AvgWaitSecs,
        travel_secs = AvgTravelSecs
      }] ++ SconnsAcc
    end,
    [],
    SegmentsMap
  ).

route_of_trip_id(RouteTableId, TripTableId, TripId) ->
  [Trip] = ets:lookup(TripTableId, TripId),
  lists:nth(1, ets:lookup(RouteTableId, Trip#trip.route_id)).

assemble_test() ->
  RouteTableId = transit_routes_data:make_table(),
  SegmentTableId = transit_segments_data:make_table(),
  TripTableId = transit_trips_data:make_table(),

  ets:insert(RouteTableId, #route{
    id = <<"route-id">>,
    short_name = <<"route short name">>
  }),
  ets:insert(SegmentTableId, #segment{
    from_time = 11100,
    from_stop_id = "stop-A",
    to_time = 11200,
    to_stop_id = "stop-B",
    travel_secs = 90,
    trip_id = <<"trip-1">>
  }),
  ets:insert(SegmentTableId, #segment{
    from_time = 11300,
    from_stop_id = "stop-A",
    to_time = 11400,
    to_stop_id = "stop-B",
    travel_secs = 80,
    trip_id = <<"trip-2">>
  }),
  ets:insert(TripTableId, #trip{
    trip_id = <<"trip-1">>,
    route_id = <<"route-id">>
  }),
  ets:insert(TripTableId, #trip{
    trip_id = <<"trip-2">>,
    route_id = <<"route-id">>
  }),

  ?assertEqual([#sconn{
    from_stop_id = "stop-A",
    to_stop_id = "stop-B",
    transit_mode = <<"route short name">>,
    wait_secs = 24*60*60 / 2,
    travel_secs = 85.0
  }], assemble(RouteTableId, SegmentTableId, TripTableId)).

insert_to_table([Sconn|Rest], TableId) ->
  true = ets:insert(TableId, Sconn),
  insert_to_table(Rest, TableId);
insert_to_table([], _) -> ok.
