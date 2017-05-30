-include("./records/coords.hrl").
-include("./records/stop.hrl").
-include("./records/sconn.hrl").

%            +---+     +---+     +---+     +---+     +---+
%            | A |     | B |     | C |     | D |     | E |
%            +---+     +---+     +---+     +---+     +---+
%     walking: -----8---------8---------8---------8-----
%
%    red line: x----5----x----5----x----8----x----8----x
%     \ wait 20 mins
% yellow line:           x----5----x----8----x----8----x
%     \ wait 8 mins
%  green line:                     x---------5---------x
%     \ wait 4 mins
%
% We want the optimal route from A to E to be:
%  - walk to B, because the red line has too long of a wait
%  - at B, take the yellow line
%  - at C, transfer to green line because it's faster than the yellow line
%
% Stops' coordinates are zig-zagged (on longitude) so that
% walking A-B-C is necessarily longer than walking A-C.
%
% Test walk start: (47, -122).  From this, stops A and B are
% reachable by walking.

-define(TEST_STOP_A, #stop{
  id=stopA, name = <<"NW 100th Pl & 7th Ave NW (28010)">>,
  coords=#coords{lat=47.001, lon=-122}}).
-define(TEST_STOP_B, #stop{
  id=stopB, name = <<"NW 103rd St & 3rd Ave NW (28000)">>,
  coords=#coords{lat=47.007, lon=-122.0001}}).
-define(TEST_STOP_C, #stop{
  id=stopC, name = <<"3rd Ave & Cedar St (2220)">>,
  coords=#coords{lat=47.013, lon=-122}}).
-define(TEST_STOP_D, #stop{
  id=stopD, name = <<"3rd Ave & Union St (450)">>,
  coords=#coords{lat=47.019, lon=-122.0001}}).
-define(TEST_STOP_E, #stop{
  id=stopE, name = <<"S Jackson St & 5th Ave S (1471)">>,
  coords=#coords{lat=47.025, lon=-122}}).

% Red line
-define(TEST_SCONN_RED_A_B, #sconn{
  from_stop_id=stopA,
  to_stop_id=stopB,
  transit_mode=routeRed,
  wait_mins=20,
  travel_mins=5}).
-define(TEST_SCONN_RED_B_C, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopC,
  transit_mode=routeRed,
  wait_mins=20,
  travel_mins=5}).
-define(TEST_SCONN_RED_C_D, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopD,
  transit_mode=routeRed,
  wait_mins=20,
  travel_mins=8}).
-define(TEST_SCONN_RED_D_E, #sconn{
  from_stop_id=stopD,
  to_stop_id=stopE,
  transit_mode=routeRed,
  wait_mins=20,
  travel_mins=8}).

% Yellow line
-define(TEST_SCONN_YELLOW_B_C, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopC,
  transit_mode=routeYellow,
  wait_mins=8,
  travel_mins=5}).
-define(TEST_SCONN_YELLOW_C_D, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopD,
  transit_mode=routeYellow,
  wait_mins=8,
  travel_mins=8}).
-define(TEST_SCONN_YELLOW_D_E, #sconn{
  from_stop_id=stopD,
  to_stop_id=stopE,
  transit_mode=routeYellow,
  wait_mins=8,
  travel_mins=8}).

% Green line
-define(TEST_SCONN_GREEN_C_E, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopE,
  transit_mode=routeGreen,
  wait_mins=3,
  travel_mins=5}).
