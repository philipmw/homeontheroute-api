-module(homeontheroute_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(TransitTableId) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [TransitTableId]).

init(TransitTableId) ->
  {ok,
    {
      #{ strategy => one_for_one,
        intensity => 2,
        period => 3600 },
      [
        #{ id => webserver,
          start => {webserver, start, TransitTableId},
          restart => permanent,
          shutdown => 1000,
          type => worker,
          modules => [cowboy, webserver] },

        #{ id => visitor_counter,
          start => {visitor_counter_server, start, []},
          restart => permanent,
          shutdown => 1000,
          type => worker,
          modules => [visitor_counter_server] }
      ]
    }
  }.
