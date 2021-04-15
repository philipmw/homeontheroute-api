-ifndef(TRIP_HRL).
-define(TRIP_HRL, 1).

-record(trip, {
  trip_id,
  route_id
}).

-record(trip_config, {
  tabs,
  log,
  stopZid,
  totalSecsAllowed,
  totalTransfersAllowed
}).

-record(trip_result, {
  optimalTrip,
  fnInstQty
}).

-endif.
