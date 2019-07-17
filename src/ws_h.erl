-module(ws_h).
-include_lib("kernel/include/logger.hrl").
-export([init/2]).
-export([websocket_init/1, websocket_info/2, websocket_handle/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  gproc:reg({p, l, operator}),
  {ok, State}.

websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info(Msg = {device_connected, VDeviceId}, State) ->
  ?LOG_NOTICE("ws_h received: ~p", [Msg]),
  Text = io_lib:format("~s", [["device_connected", " ", erlang:integer_to_list(VDeviceId)]]),
  {reply, {text, Text}, State};

websocket_info(Msg = {device_data_recv, VDeviceId, Timestamp, Value}, State) ->
  ?LOG_NOTICE("ws_h received: ~p", [Msg]),
  Text = io_lib:format("~s", [[ "device_data_recv"
                              , " "
                              , erlang:integer_to_list(VDeviceId)
                              , " "
                              , erlang:integer_to_list(Timestamp)
                              , " "
                              , erlang:integer_to_list(Value)
                              ]]),
  {reply, {text, Text}, State};

websocket_info(Info, State) ->
  ?LOG_NOTICE("ws_h received: ~p", [Info]),
  {ok, State}.
