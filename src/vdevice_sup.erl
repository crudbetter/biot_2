-module(vdevice_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").
-export([start_link/0, start_or_get/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, vdevice_sup}, ?MODULE, []).

start_or_get(VDeviceId) ->
  case vdevice:lookup(VDeviceId) of
    undefined ->
        ?LOG_NOTICE("Starting vdevice with id: ~p", [VDeviceId]),
        supervisor:start_child(vdevice_sup, [VDeviceId]);
    VDevice ->
        {ok, VDevice}
  end.

init(_Args) ->
  SupFlags = #{ strategy => simple_one_for_one
              },
  ChildSpecs = [#{ id => vdevice
                 , start => {vdevice, start_link, []}
                 }],
  {ok, {SupFlags, ChildSpecs}}.
