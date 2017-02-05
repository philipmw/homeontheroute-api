-include("./records/stop.hrl").
-include("./records/sconn.hrl").

-define(TEST_STOP_A, #stop{
  id=stopA, name = <<"NW 103rd St & 3rd Ave NW (28000)">>,
  lat=47.7035561, lon=-122.361244}).
-define(TEST_STOP_B, #stop{
  id=stopB, name = <<"3rd Ave & Cedar St (2220)">>,
  lat=47.6168709, lon=-122.348625}).
-define(TEST_STOP_C, #stop{
  id=stopC, name = <<"3rd Ave & Union St (450)">>,
  lat=47.6082497, lon=-122.336548}).
-define(TEST_STOP_D, #stop{
  id=stopD, name = <<"S Jackson St & 5th Ave S (1471)">>,
  lat=47.5991249, lon=-122.328041}).

-define(TEST_SCONN_B_C, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopC,
  transit_mode=route28,
  wait_mins=2,
  travel_mins=2}).
-define(TEST_SCONN_B_D, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopD,
  transit_mode=route29,
  wait_mins=10,
  travel_mins=4}).
