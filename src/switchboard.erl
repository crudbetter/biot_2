-module(switchboard).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").
-export([start_link/1, handle_info/2]).
-export([init/1]).

start_link(Port) ->
  gen_server:start_link(?MODULE, Port, []).

init(Port) ->
  {ok, ListenSocket} =
    gen_tcp:listen(Port, [{active, true}, binary, {packet, 1}, {reuseaddr, true}]),

  ?LOG_NOTICE("Accepting connections on: ~p", [Port]),

  self() ! loop_acceptor,

  {ok, #{ listenSocket => ListenSocket
        }}.

handle_info(loop_acceptor, State = #{ listenSocket := ListenSocket
                                    }) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),

  {ok, Operator} = supervisor:start_child(operator_sup, [AcceptSocket]),

  ok = gen_tcp:controlling_process(AcceptSocket, Operator),

  self() ! loop_acceptor,

  {noreply, State}.
