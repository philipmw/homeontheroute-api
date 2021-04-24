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

% `trip_config` is for invariants of `optimal_trip()`.  For changing values, use `trip_state`.
-record(trip_config, {
  tabs,
  log,
  stopZid,
  totalTransfersAllowed
}).

% `trip_state` is for changing values of `optimal_trip()`.  For invariants, use `trip_config`.
-record(trip_state, {
  % List of trip segments, in reverse chronological order.  It is initialized by the customer
  % to a single segment, being an 0-minute walk to the initial stop.
  segs,
  % Amount of time the trip is allowed to take, inclusive of `segs`.
  %
  % This value is initialized by the customer (given that `segs` has initial time of 0),
  % but then is reduced to the time required to direct-walk, as an optimization.
  remainAllowedSecs
}).

-record(trip_result, {
  optimalTrip,
  fnInstQty
}).

-endif.
