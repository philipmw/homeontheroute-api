-include("./records/stop.hrl").
-include("./records/sconn.hrl").

%            +---+     +---+     +---+     +---+     +---+
%            | A |     | B |     | C |     | D |     | E |
%            +---+     +---+     +---+     +---+     +---+
%    red line: x----5----x----5----x----8----x----8----x
%     \ comes every 30 mins
% yellow line:           x----5----x----8----x----8----x
%     \ comes every 15 mins
%  green line:                     x---------5---------x
%     \ comes every 5 mins
%
% We want the optimal route from A to E to be:
%  - walk to B, because the red line has too long of a wait
%  - at B, take the yellow line
%  - at C, transfer to green line because it's faster than the yellow line

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

% Red line
-define(TEST_SCONN_RED_A_B, #sconn{
  from_stop_id=stopA,
  to_stop_id=stopB,
  transit_mode=routeRed,
  wait_mins=30,
  travel_mins=5}).
-define(TEST_SCONN_RED_B_C, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopC,
  transit_mode=routeRed,
  wait_mins=30,
  travel_mins=5}).
-define(TEST_SCONN_RED_C_D, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopD,
  transit_mode=routeRed,
  wait_mins=30,
  travel_mins=8}).
-define(TEST_SCONN_RED_D_E, #sconn{
  from_stop_id=stopD,
  to_stop_id=stopE,
  transit_mode=routeRed,
  wait_mins=30,
  travel_mins=8}).

% Yellow line
-define(TEST_SCONN_YELLOW_B_C, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopC,
  transit_mode=routeYellow,
  wait_mins=15,
  travel_mins=5}).
-define(TEST_SCONN_YELLOW_C_D, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopD,
  transit_mode=routeYellow,
  wait_mins=15,
  travel_mins=8}).
-define(TEST_SCONN_YELLOW_D_E, #sconn{
  from_stop_id=stopD,
  to_stop_id=stopE,
  transit_mode=routeYellow,
  wait_mins=15,
  travel_mins=8}).

% Green line
-define(TEST_SCONN_GREEN_C_E, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopE,
  transit_mode=routeGreen,
  wait_mins=5,
  travel_mins=5}).
