-module(callcentre_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, callcentre_sup}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{ strategy => one_for_one
              },
  ChildSpecs = [ #{ id => switchboard
                  , start => {switchboard, start_link, [4040]}
                  }
               , #{ id => operator_sup
                  , start => {operator_sup, start_link, []}
                  , type => supervisor
                  }
               , #{ id => vdevice_sup
                  , start => {vdevice_sup, start_link, []}
                  , type => supervisor
                  }],
  {ok, {SupFlags, ChildSpecs}}.
