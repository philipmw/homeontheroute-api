-include("./records/coords.hrl").
-include("./records/stop.hrl").
-include("./records/sconn.hrl").

% Stop connectivity:
%
%   A -> B -> C -> D -> E <> F <> Z
%         ^            /
%          \          v
%           I <- H <- G
%
% Walking:
%   A-B: 8, B-C: 8, C-D: 8, D-E: 8, E-F: 8, F-Z: 60,
%     E-G: 6, G-H: 8, H-I: 8, I-B: 6
%
% Red line:
%   wait 10, A->B: 3
%
% Yellow line:
%   wait 5, B->C: 3, C->D: 3, D->E: 3, E->F: 3
%
% Green line:
%   wait 3, C->F: 3
%
% Blue line:
%   wait 3, Z->F: 5, F->E: 2, E->G: 2, G->H: 2, H->I: 2, I->B: 2
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
-define(TEST_STOP_G, #stop{
  id=stopG, name = <<"Stop G">>,
  coords=#coords{lat=47.025, lon=-122.02}}).
-define(TEST_STOP_H, #stop{
  id=stopH, name = <<"Stop H">>,
  coords=#coords{lat=47.016, lon=-122.0201}}).
-define(TEST_STOP_I, #stop{
  id=stopI, name = <<"Stop I">>,
  coords=#coords{lat=47.010, lon=-122.02}}).
-define(TEST_STOP_Z, #stop{
  id=stopZ, name = <<"Stop Z">>,
  coords=#coords{lat=47.077, lon=-122}}).

% Red line
-define(TEST_SCONN_RED_A_B, #sconn{
  from_stop_id=stopA,
  to_stop_id=stopB,
  transit_mode=routeRed,
  wait_secs=60*10,
  travel_secs=60*4}).

% Yellow line
-define(TEST_SCONN_YELLOW_B_C, #sconn{
  from_stop_id=stopB,
  to_stop_id=stopC,
  transit_mode=routeYellow,
  wait_secs=60*5,
  travel_secs=60*3}).
-define(TEST_SCONN_YELLOW_C_D, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopD,
  transit_mode=routeYellow,
  wait_secs=60*5,
  travel_secs=60*3}).
-define(TEST_SCONN_YELLOW_D_E, #sconn{
  from_stop_id=stopD,
  to_stop_id=stopE,
  transit_mode=routeYellow,
  wait_secs=60*5,
  travel_secs=60*3}).
-define(TEST_SCONN_YELLOW_E_F, #sconn{
  from_stop_id=stopE,
  to_stop_id=stopF,
  transit_mode=routeYellow,
  wait_secs=60*5,
  travel_secs=60*3}).

% Green line
-define(TEST_SCONN_GREEN_C_F, #sconn{
  from_stop_id=stopC,
  to_stop_id=stopF,
  transit_mode=routeGreen,
  wait_secs=60*3,
  travel_secs=60*3}).

% Blue line
-define(TEST_SCONN_BLUE_Z_F, #sconn{
  from_stop_id=stopZ,
  to_stop_id=stopF,
  transit_mode=routeBlue,
  wait_secs=60*3,
  travel_secs=60*5}).
-define(TEST_SCONN_BLUE_F_E, #sconn{
  from_stop_id=stopF,
  to_stop_id=stopE,
  transit_mode=routeBlue,
  wait_secs=60*3,
  travel_secs=60*2}).
-define(TEST_SCONN_BLUE_E_G, #sconn{
  from_stop_id=stopE,
  to_stop_id=stopG,
  transit_mode=routeBlue,
  wait_secs=60*3,
  travel_secs=60*2}).
-define(TEST_SCONN_BLUE_G_H, #sconn{
  from_stop_id=stopG,
  to_stop_id=stopH,
  transit_mode=routeBlue,
  wait_secs=60*3,
  travel_secs=60*2}).
-define(TEST_SCONN_BLUE_H_I, #sconn{
  from_stop_id=stopH,
  to_stop_id=stopI,
  transit_mode=routeBlue,
  wait_secs=60*3,
  travel_secs=60*3}).
-define(TEST_SCONN_BLUE_I_B, #sconn{
  from_stop_id=stopI,
  to_stop_id=stopB,
  transit_mode=routeBlue,
  wait_secs=60*3,
  travel_secs=60*2}).
