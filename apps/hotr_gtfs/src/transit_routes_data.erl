-module(transit_routes_data).
-export([
  insert_to_table/2,
  load_from_file/1,
  make_table/0
]).

-include_lib("eunit/include/eunit.hrl").
-include("records/route.hrl").

make_table() ->
  ets:new(routes, [{read_concurrency, true}, {keypos, #route.id}]).

load_from_file(Filename) ->
  io:fwrite("Loading routes from ~s~n", [Filename]),
  {ok, DataBinary} = file:read_file(Filename),
  DataBinaryList = binary:split(DataBinary, <<$\n>>, [global]),
  [fileline_to_route(B) || B <- select_good_lines(DataBinaryList)].

load_from_file_test() ->
  Routes = load_from_file(gtfs:filename_for(routes)),
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

select_good_lines(DataBinaryList) ->
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

