% `trip.hrl` is exlusively for GTFS "trip".
% Also see `optimal_trip.hrl`.

-ifndef(TRIP_HRL).
-define(TRIP_HRL, 1).

-record(trip, {
  trip_id,
  route_id
}).

-endif.
