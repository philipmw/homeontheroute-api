-include("./records/stop.hrl").
-include("./records/sconn.hrl").

-define(TEST_STOP_A, #stop{
  id=stopA, name = <<"NW 100th Pl & 7th Ave NW (28010)">>,
  lat=47.7026291, lon=-122.363785}).
-define(TEST_STOP_B, #stop{
  id=stopB, name = <<"NW 103rd St & 3rd Ave NW (28000)">>,
  lat=47.7035561, lon=-122.361244}).
-define(TEST_STOP_C, #stop{
  id=stopC, name = <<"3rd Ave & Cedar St (2220)">>,
  lat=47.6168709, lon=-122.348625}).
-define(TEST_STOP_D, #stop{
  id=stopD, name = <<"3rd Ave & Union St (450)">>,
  lat=47.6082497, lon=-122.336548}).
-define(TEST_STOP_E, #stop{
  id=stopE, name = <<"S Jackson St & 5th Ave S (1471)">>,
  lat=47.5991249, lon=-122.328041}).

-define(TEST_SCONN_B_C, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopC,
  transit_mode=routeBC,
  wait_mins=2,
  travel_mins=30}).
-define(TEST_SCONN_B_D, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopD,
  transit_mode=routeLongWait,
  wait_mins=15,
  travel_mins=30}).
-define(TEST_SCONN_C_D, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopD,
  transit_mode=routeCD,
  wait_mins=3,
  travel_mins=15}).
-define(TEST_SCONN_D_E, #sconn{
  from_stop_id=stopD,
  to_stop_id=stopE,
  transit_mode=routeDE,
  wait_mins=2,
  travel_mins=10}).
