-ifndef(SCONN_HRL).
-define(SCONN_HRL, 1).

-record(sconn, {
  from_stop_id,
  to_stop_id,
  transit_mode,
  wait_secs,
  travel_secs
}).

-type sconn() :: #sconn{}.

-endif.
