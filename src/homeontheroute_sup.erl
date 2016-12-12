%%%-------------------------------------------------------------------
%% @doc homeontheroute top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(homeontheroute_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok,
    {
      #{ strategy => one_for_one,
        intensity => 2,
        period => 3600 },
      [
        #{ id => webserver,
          start => {webserver, start, []},
          restart => permanent,
          shutdown => 1000,
          type => worker,
          modules => [cowboy, webserver] },

        #{ id => visitor_counter,
          start => {visitor_counter_server, start, []},
          restart => permanent,
          shutdown => 1000,
          type => worker,
          modules => [visitor_counter_server] },

        #{ id => transit_server,
          start => {transit_server, start, []},
          restart => permanent,
          shutdown => 1000,
          type => worker,
          modules => [transit_server] }
      ]
    }
  }.

%%====================================================================
%% Internal functions
%%====================================================================
