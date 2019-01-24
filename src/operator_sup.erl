-module(operator_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, operator_sup}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{ strategy => simple_one_for_one
              },
  ChildSpecs = [#{ id => operator
                 , start => {operator, start_link, []}
                 , restart => transient
                 }],
  {ok, {SupFlags, ChildSpecs}}.
