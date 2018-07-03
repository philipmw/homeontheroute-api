-include("./records/coords.hrl").
-include("./records/stop.hrl").
-include("./records/sconn.hrl").

%            +---+     +---+     +---+     +---+     +---+     +---+     +---+
%            | A |     | B |     | C |     | D |     | E |     | F |     | Z |
%            +---+     +---+     +---+     +---+     +---+     +---+     +---+
%     walking: -----8---------8---------8---------8---------8---------60----
%
%    red line: x----3----x
%     \ wait 10 mins
% yellow line:           x----3----x----3----x----3----x----3----x
%     \ wait 5 mins
%  green line:                     x--------------3--------------x
%     \ wait 3 mins
%
% We want the optimal route from A to F to be:
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
  id=stopA, name = <<"Stop A">>,
  coords=#coords{lat=47.001, lon=-122}}).
-define(TEST_STOP_B, #stop{
  id=stopB, name = <<"Stop B">>,
  coords=#coords{lat=47.007, lon=-122.0001}}).
-define(TEST_STOP_C, #stop{
  id=stopC, name = <<"Stop C">>,
  coords=#coords{lat=47.013, lon=-122}}).
-define(TEST_STOP_D, #stop{
  id=stopD, name = <<"Stop D">>,
  coords=#coords{lat=47.019, lon=-122.0001}}).
-define(TEST_STOP_E, #stop{
  id=stopE, name = <<"Stop E">>,
  coords=#coords{lat=47.025, lon=-122}}).
-define(TEST_STOP_F, #stop{
  id=stopF, name = <<"Stop F">>,
  coords=#coords{lat=47.031, lon=-122.0001}}).
-define(TEST_STOP_Z, #stop{
  id=stopZ, name = <<"Stop Z">>,
  coords=#coords{lat=49, lon=-122}}).

% Red line
-define(TEST_SCONN_RED_A_B, #sconn{
  from_stop_id=stopA,
  to_stop_id=stopB,
  transit_mode=routeRed,
  wait_mins=10,
  travel_mins=4}).

% Yellow line
-define(TEST_SCONN_YELLOW_B_C, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopC,
  transit_mode=routeYellow,
  wait_mins=5,
  travel_mins=3}).
-define(TEST_SCONN_YELLOW_C_D, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopD,
  transit_mode=routeYellow,
  wait_mins=5,
  travel_mins=3}).
-define(TEST_SCONN_YELLOW_D_E, #sconn{
  from_stop_id=stopD,
  to_stop_id=stopE,
  transit_mode=routeYellow,
  wait_mins=5,
  travel_mins=3}).
-define(TEST_SCONN_YELLOW_E_F, #sconn{
  from_stop_id=stopE,
  to_stop_id=stopF,
  transit_mode=routeYellow,
  wait_mins=5,
  travel_mins=3}).

% Green line
-define(TEST_SCONN_GREEN_C_F, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopF,
  transit_mode=routeGreen,
  wait_mins=3,
  travel_mins=3}).
