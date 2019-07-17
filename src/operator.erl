-module(operator).
-behaviour(gen_statem).
-include_lib("kernel/include/logger.hrl").
-export([start_link/1, handle_event/4]).
-export([init/1, callback_mode/0]).

start_link(Socket) ->
  gen_statem:start_link({global, {?MODULE, Socket}}, ?MODULE, Socket, []).

handle(Operator, Data) ->
  gen_statem:call(Operator, Data).

init(Socket) ->
  {ok, ready, #{ socket => Socket
               , how_many => undefined
               , vdevice => undefined
               }}.

callback_mode() ->
  handle_event_function.

handle_event(info, {tcp, Socket, <<1:8, VDeviceId:16, HowMany:16>>}, ready, Data = #{ socket := Socket
                                                                                    }) ->
  {ok, VDevice} = vdevice_sup:start_or_get(VDeviceId),

  gen_tcp:send(Socket, <<2:8>>),

  gproc:send({p, l, operator}, {device_connected, VDeviceId}),

  {next_state, connected, Data#{ vdeviceId => VDeviceId
                               , vdevice => VDevice
                               , how_many => HowMany
                               }};

handle_event(info, {tcp, Socket, <<5:8>>}, connected, #{ socket := Socket
                                                       , how_many := 0
                                                       }) ->
  {stop, normal};

handle_event(info, {tcp, Socket, <<3:8, Timestamp:64, Value:64>>}, connected, Data = #{ socket := Socket
                                                                                      , how_many := HowMany
                                                                                      , vdeviceId := VDeviceId
                                                                                      , vdevice := VDevice
                                                                                      }) ->
  ok = vdevice:push(VDevice, [Timestamp, Value]),

  gen_tcp:send(Socket, <<4:8>>),

  gproc:send({p, l, operator}, {device_data_recv, VDeviceId, Timestamp, Value}),

  {next_state, connected, Data#{ how_many => HowMany - 1
                               }};

handle_event(info, {tcp_closed, Socket}, connected, #{ socket := Socket
                                                     }) ->
  {stop, normal}.
