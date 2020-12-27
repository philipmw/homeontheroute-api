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
  lists:map(fun(Seg) -> segment_to_sconn(Seg, RouteTableId, TripTableId) end, Segments).

segment_to_sconn(Segment, RouteTableId, TripTableId) ->
  Route = route_of_trip_id(RouteTableId, TripTableId, Segment#segment.trip_id),
  #sconn{
    from_stop_id = Segment#segment.from_stop_id,
    to_stop_id = Segment#segment.to_stop_id,
    transit_mode = Route#route.short_name,
    wait_secs = 60*5, % FIXME
    travel_secs = Segment#segment.travel_secs
  }.

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
    trip_id = <<"trip-id">>
  }),
  ets:insert(TripTableId, #trip{
    trip_id = <<"trip-id">>,
    route_id = <<"route-id">>
  }),

  ?assertEqual([#sconn{
    from_stop_id = "stop-A",
    to_stop_id = "stop-B",
    transit_mode = <<"route short name">>,
    wait_secs = 0, % FIXME
    travel_secs = 90
  }], assemble(RouteTableId, SegmentTableId, TripTableId)).

insert_to_table([Sconn|Rest], TableId) ->
  true = ets:insert(TableId, Sconn),
  insert_to_table(Rest, TableId);
insert_to_table([], _) -> ok.
