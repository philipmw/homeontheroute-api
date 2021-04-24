-ifndef(OPTIMAL_TRIP_HRL).
-define(OPTIMAL_TRIP_HRL, 1).

-record(trip_ends, {
  from_stop_id,
  to_stop_id
}).

-record(optimal_trip, {
  trip_ends,
  trip_result,
  trip_secs,
  compute_secs
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
