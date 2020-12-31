-module(gtfs).

-export([filename_for/1]).

-ifndef(GTFS_BASEDIR).
-define(GTFS_BASEDIR, "metro-gtfs").
-endif.

filename_for(Type) ->
%%  case application:get_application() of
%%               {ok, AppName} -> code:priv_dir(AppName) ++ "/" ++ GtfsBasedir ++ "/stops.txt";
%%               _ -> "./priv/" ++ GtfsBasedir ++ "/stops.txt"
%%             end,
  "./apps/hotr_gtfs/priv/" ++ ?GTFS_BASEDIR ++ "/" ++ erl_types:atom_to_string(Type) ++ ".txt".

