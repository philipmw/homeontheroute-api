-ifndef(STOP_TIME_HRL).
-define(STOP_TIME_HRL, 1).

-record(stop_time, {
  trip_id,
  arr_time,
  dep_time,
  stop_id,
  stop_seq
}).

-endif.
