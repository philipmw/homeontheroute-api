-module(transit_segments_data).
-export([
  insert_to_table/2,
  load_from_file/1,
  make_table/0
]).

-include_lib("eunit/include/eunit.hrl").
-include("gtfs.hrl").
-include("records/segment.hrl").
-include("records/stop_time.hrl").

make_table() ->
  ets:new(segments, [bag, {keypos, #segment.from_stop_id}]).

load_from_file(GtfsBasedir) ->
  StopTimes = load_stop_times_from_file(GtfsBasedir),
  io:fwrite("Read ~B stop-times from file; assembling them into segments~n", [length(StopTimes)]),
  MapByTripId = partition_by_trip_id(StopTimes),
  parallel_convert_stop_times_to_segments(MapByTripId).

load_stop_times_from_file(GtfsBasedir) ->
  Filename = case application:get_application() of
               {ok, AppName} -> code:priv_dir(AppName) ++ "/" ++ GtfsBasedir ++ "/stop_times.txt";
               _ -> "./priv/" ++ GtfsBasedir ++ "/stop_times.txt"
             end,
  io:fwrite("Loading stop times from ~s~n", [Filename]),
  {ok, DataBinary} = file:read_file(Filename),
  DataBinaryList = binary:split(DataBinary, <<$\n>>, [global]),
  [fileline_to_stop_time(B) || B <- select_good_lines(DataBinaryList)].

load_stop_times_from_file_test() ->
  Segments = load_stop_times_from_file(?GTFS_BASEDIR),
  [Segment|_] = Segments,
  ?assertEqual(Segment, #stop_time{
    trip_id = <<"34745815">>,
    arr_time = 26340, % from 07:19:00
    dep_time = 26340, % from 07:19:00
    stop_id = <<"64592">>,
    stop_seq = 1
  }).

partition_by_trip_id(StopTimes) ->
  lists:foldl(
    fun (StopTime, MapByTripId) ->
      maps:update_with(
        StopTime#stop_time.trip_id,
        fun (CurrList) -> CurrList ++ [StopTime] end,
        [StopTime],
        MapByTripId)
    end,
    maps:new(),
    StopTimes).

partition_by_trip_id_test() ->
  ?assertEqual(
    #{<<"trip-1">> => [
      #stop_time{trip_id = <<"trip-1">>, stop_id = "stop-A"},
      #stop_time{trip_id = <<"trip-1">>, stop_id = "stop-B"}
    ], <<"trip-2">> => [
      #stop_time{trip_id = <<"trip-2">>, stop_id = "stop-C"}
    ]},
    partition_by_trip_id([
      #stop_time{trip_id = <<"trip-1">>, stop_id = "stop-A"},
      #stop_time{trip_id = <<"trip-1">>, stop_id = "stop-B"},
      #stop_time{trip_id = <<"trip-2">>, stop_id = "stop-C"}
    ])
  ).

convert_stop_times_to_segments([]) -> [];
convert_stop_times_to_segments([_]) -> {error, onlyonesegment};
convert_stop_times_to_segments([StopTime0|StopTimesTL]) ->
%%  io:fwrite("convert_stop_times_to_segments([~B elements]) started for trip ID ~s~n",
%%    [length(StopTimesTL)+1, StopTime0#stop_time.trip_id]),
  % accumulator: {
  %   is everything ok?
  %   last-stop-time,
  %   segments
  % }
  {ok, _, Segments} = lists:foldl(
    fun (StopTime, {ok, PrevStopTime, Segments}) ->
      if PrevStopTime#stop_time.trip_id == StopTime#stop_time.trip_id andalso
        PrevStopTime#stop_time.stop_seq < StopTime#stop_time.stop_seq  ->
        % we can form a new segment
%%        io:fwrite("Making a new segment for trip ID ~s, stops ~s -> ~s~n",
%%          [StopTime#stop_time.trip_id, PrevStopTime#stop_time.stop_id, StopTime#stop_time.stop_id]),
        {ok, StopTime, Segments ++ [#segment{
          from_time = PrevStopTime#stop_time.dep_time,
          from_stop_id = PrevStopTime#stop_time.stop_id,
          from_stop_seq = PrevStopTime#stop_time.stop_seq,
          to_time = StopTime#stop_time.arr_time,
          to_stop_id = StopTime#stop_time.stop_id,
          to_stop_seq = StopTime#stop_time.stop_seq,
          trip_id = StopTime#stop_time.trip_id,
          travel_secs = StopTime#stop_time.arr_time - PrevStopTime#stop_time.dep_time
        }]};
        PrevStopTime#stop_time.trip_id /= StopTime#stop_time.trip_id ->
          % the trip ID changed; we'll make a new segment next time
          {ok, StopTime, Segments};
        true ->
          % most likely the sequence ID is not incrementing in the same trip
          {error, StopTime, Segments}
      end
    end,
    {ok, StopTime0, []},
    StopTimesTL),
  Segments.

convert_stop_times_to_segments_good_test() ->
  ?assertEqual([
    #segment{
      from_time = 32400,
      from_stop_id = "stop-A",
      from_stop_seq = 1,
      to_time = 32500,
      to_stop_id = "stop-B",
      to_stop_seq = 10,
      trip_id = <<"1">>,
      travel_secs = 100
    },
    #segment{
      from_time = 32500,
      from_stop_id = "stop-B",
      from_stop_seq = 10,
      to_time = 32700,
      to_stop_id = "stop-C",
      to_stop_seq = 15,
      trip_id = <<"1">>,
      travel_secs = 200
    },
    #segment{
      from_time = 28800,
      from_stop_id = "stop-F",
      from_stop_seq = 1,
      to_time = 28880,
      to_stop_id = "stop-G",
      to_stop_seq = 2,
      trip_id = <<"2">>,
      travel_secs = 80
    }
  ], convert_stop_times_to_segments([
    #stop_time{
      trip_id = <<"1">>,
      arr_time = 32400, % 9:00:00 AM
      dep_time = 32400,
      stop_id = "stop-A",
      stop_seq = 1
    },
    #stop_time{
      trip_id = <<"1">>,
      arr_time = 32500, % 9:01:40 AM
      dep_time = 32500,
      stop_id = "stop-B",
      stop_seq = 10
    },
    #stop_time{
      trip_id = <<"1">>,
      arr_time = 32700, % 9:05:00 AM
      dep_time = 32700,
      stop_id = "stop-C",
      stop_seq = 15
    },
    #stop_time{
      trip_id = <<"2">>,
      arr_time = 28800, % 8:00:00 AM
      dep_time = 28800,
      stop_id = "stop-F",
      stop_seq = 1
    },
    #stop_time{
      trip_id = <<"2">>,
      arr_time = 28880, % 8:01:20 AM
      dep_time = 28880,
      stop_id = "stop-G",
      stop_seq = 2
    }
  ])).

convert_stop_times_to_segments_bad_seq_test() ->
  ?assertError({badmatch, _}, convert_stop_times_to_segments([
    #stop_time{
      trip_id = <<"trip1">>,
      arr_time = 32400, % 9:00:00 AM
      dep_time = 32400,
      stop_id = "stop-A",
      stop_seq = 10
    },
    #stop_time{
      trip_id = <<"trip1">>,
      arr_time = 32500, % 9:01:40 AM
      dep_time = 32500,
      stop_id = "stop-B",
      stop_seq = 1
    }
  ])).

parallel_convert_stop_times_to_segments(MapByTripId) ->
  lists:flatten(
    parallel:parmap(fun convert_stop_times_to_segments/1, maps:values(MapByTripId))
  ).

parallel_convert_stop_times_to_segments_test() ->
  ?assertEqual([
    #segment{
      from_time = 32400,
      from_stop_id = "stop-A",
      from_stop_seq = 1,
      to_time = 32500,
      to_stop_id = "stop-B",
      to_stop_seq = 2,
      trip_id = <<"trip-1">>,
      travel_secs = 100
    },
    #segment{
      from_time = 100,
      from_stop_id = "stop-C",
      from_stop_seq = 1,
      to_time = 200,
      to_stop_id = "stop-D",
      to_stop_seq = 2,
      trip_id = <<"trip-2">>,
      travel_secs = 100
    },
    #segment{
      from_time = 300,
      from_stop_id = "stop-D",
      from_stop_seq = 2,
      to_time = 400,
      to_stop_id = "stop-E",
      to_stop_seq = 3,
      trip_id = <<"trip-2">>,
      travel_secs = 100
    }
  ], parallel_convert_stop_times_to_segments(
    #{<<"trip-1">> => [
      #stop_time{trip_id = <<"trip-1">>, stop_id = "stop-A", stop_seq = 1, dep_time = 32400},
      #stop_time{trip_id = <<"trip-1">>, stop_id = "stop-B", stop_seq = 2, arr_time = 32500}
    ], <<"trip-2">> => [
      #stop_time{trip_id = <<"trip-2">>, stop_id = "stop-C", stop_seq = 1, arr_time = 100, dep_time = 100},
      #stop_time{trip_id = <<"trip-2">>, stop_id = "stop-D", stop_seq = 2, arr_time = 200, dep_time = 300},
      #stop_time{trip_id = <<"trip-2">>, stop_id = "stop-E", stop_seq = 3, arr_time = 400, dep_time = 500}
    ]}
  )).

insert_to_table([Segment|Rest], SegmentsTableId) ->
  true = ets:insert(SegmentsTableId, Segment),
  insert_to_table(Rest, SegmentsTableId);
insert_to_table([], _) -> ok.

select_good_lines(DataBinaryList) ->
  % skip the header
  HeaderlessList = lists:nthtail(1, DataBinaryList),
  % skip empty lines
  lists:filter(fun(BL) -> BL /= <<>> end, HeaderlessList).

fileline_to_stop_time(BinaryLine) ->
  Fields = binary:split(BinaryLine, <<$,>>, [global]),
  #stop_time{
    trip_id = lists:nth(1, Fields),
    arr_time = binary_to_gtfs_time(lists:nth(2, Fields)),
    dep_time = binary_to_gtfs_time(lists:nth(3, Fields)),
    stop_id = lists:nth(4, Fields),
    % converting to integer so we can ensure that they're properly ordered in the file
    stop_seq = binary_to_integer(lists:nth(5, Fields))
  }.

% I choose to represent GTFS timestamps as seconds from midnight,
% as my top use case is calculating difference between two timestamps.
% Also, GTFS allows timestamps to be greater than 24:00:00 (such as 25:35:00),
% so integers make even more sense.
binary_to_gtfs_time(Bin) ->
  Fields = binary:split(Bin, <<$:>>, [global]),
  Hour = binary_to_integer(lists:nth(1, Fields)),
  Minute = binary_to_integer(lists:nth(2, Fields)),
  Second = binary_to_integer(lists:nth(3, Fields)),
  Hour * 3600 + Minute * 60 + Second.

binary_to_gtfs_time_test() ->
  ?assertEqual(92100, binary_to_gtfs_time(<<"25:35:00">>)).