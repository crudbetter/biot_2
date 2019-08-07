-module(callcentre).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/", cowboy_static, {priv_file, biot, "index.html"}},
                                           {"/websocket", ws_h, []},
                                           {"/static/[...]", cowboy_static, {priv_dir, biot, "static"}}
                                          ]}
                                   ]),
  {ok, _} = cowboy:start_clear(http, [{port, 8081}], #{
                                                       env => #{dispatch => Dispatch}
                                                      }),

  callcentre_sup:start_link().

stop(_State) ->
  ok.
