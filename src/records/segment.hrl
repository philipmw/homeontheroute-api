-ifndef(SEGMENT_HRL).
-define(SEGMENT_HRL, 1).

-record(segment, {
  from_time,
  from_stop_id,
  from_stop_seq,
  to_time,
  to_stop_id,
  to_stop_seq,
  trip_id,
  travel_secs
}).

-type segment() :: #segment{}.

-endif.
