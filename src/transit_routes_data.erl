-module(transit_routes_data).
-export([
  insert_to_table/2,
  load_from_file/1
]).

-include_lib("eunit/include/eunit.hrl").
-include("gtfs.hrl").
-include("records/route.hrl").

load_from_file(GtfsBasedir) ->
  RoutesFilename = case application:get_application() of
                    {ok, AppName} ->
                      code:priv_dir(AppName) ++ "/" ++ GtfsBasedir ++ "/routes.txt";
                    _ ->
                      "./priv/" ++ GtfsBasedir ++ "/routes.txt"
                  end,
  io:fwrite("Loading routes from ~s~n", [RoutesFilename]),
  {ok, RoutesDataBinary} = file:read_file(RoutesFilename),
  RoutesDataBinaryList = binary:split(RoutesDataBinary, <<$\n>>, [global]),
  [fileline_to_route(B) || B <- select_route_lines(RoutesDataBinaryList)].

load_from_file_test() ->
  Routes = load_from_file(?GTFS_BASEDIR),
  [Route|_] = Routes,
  ?assertEqual(Route, #route{
    id = <<"100001">>,
    short_name = <<"1">>,
    desc = <<"Kinnear - Downtown Seattle">>
  }).

insert_to_table([Route|Rest], RoutesTableId) ->
  true = ets:insert(RoutesTableId, Route),
  insert_to_table(Rest, RoutesTableId);
insert_to_table([], _) -> ok.

select_route_lines(DataBinaryList) ->
  % skip the header
  HeaderlessList = lists:nthtail(1, DataBinaryList),
  % skip empty lines
  lists:filter(fun(BL) -> BL /= <<>> end, HeaderlessList).

fileline_to_route(BinaryLine) ->
%%  io:fwrite("Line: ~s~n", [BinaryLine]),
  Fields = binary:split(BinaryLine, <<$,>>, [global]),
%%  io:fwrite("Fields: ~s~n", [Fields]),
  #route{
    id = lists:nth(1, Fields),
    short_name = binary:replace(lists:nth(3, Fields), <<$">>, <<>>, [global]),
    desc = binary:replace(lists:nth(5, Fields), <<$">>, <<>>, [global])
  }.

