-module(vdevice).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").
-export([start_link/1, push/2, lookup/1]).
-export([init/1, handle_cast/2, handle_info/2]).

start_link(VDeviceId) ->
  gen_server:start_link({global, {?MODULE, VDeviceId}}, ?MODULE, VDeviceId, []).

push(VDevice, Head) ->
  gen_server:cast(VDevice, {push, Head}).

lookup(VDeviceId) ->
  global:whereis_name({?MODULE, VDeviceId}).

init(VDeviceId) ->
  erlang:send_after(10000, self(), flush),

  {ok, #{ vdevice_id => VDeviceId
        , buffer => []
        }}.

handle_info(flush, State = #{ vdevice_id := VDeviceId
                            }) ->
  ?LOG_NOTICE("Flushing buffer for vdevice with id: ~p", [VDeviceId]),

  Body = construct_line_protocol(State),

  httpc:request(post, {"http://127.0.0.1:8086/write?db=biot&precision=ms", [], "", Body}, [], []),

  erlang:send_after(10000, self(), flush),

  {noreply, State#{ buffer => []
                   }}.

handle_cast({push, Head}, State = #{ vdevice_id := VDeviceId
                                   , buffer := Tail
                                   }) ->
  ?LOG_NOTICE("Pushing ~p to vdevice with id: ~p", [Head, VDeviceId]),

  {noreply, State#{ buffer => [Head | Tail]
                   }}.

construct_line_protocol(#{ vdevice_id := VDeviceId
                         , buffer := Buffer
                         }) ->
  IoList = lists:foldl(fun([TS, V], Acc) ->
                           Acc ++ ["signal_raw,vdevice_id=", integer_to_binary(VDeviceId), " sample=", integer_to_binary(V), " ", integer_to_binary(TS), "\n"]
                       end, [], Buffer),
  iolist_to_binary(IoList).
