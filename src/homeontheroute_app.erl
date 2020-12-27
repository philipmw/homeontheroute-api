%%%-------------------------------------------------------------------
%% @doc homeontheroute public API
%% @end
%%%-------------------------------------------------------------------

-module(homeontheroute_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  TransitTableId = transit_data:create_all_ets(),
  homeontheroute_sup:start_link(TransitTableId).

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
