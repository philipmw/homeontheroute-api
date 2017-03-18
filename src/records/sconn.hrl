-ifndef(SCONN_HRL).
-define(SCONN_HRL, 1).

-record(sconn, {
  from_stop_id,
  to_stop_id,
  transit_mode,
  wait_mins,
  travel_mins
}).

-type sconn() :: #sconn{}.

-endif.
