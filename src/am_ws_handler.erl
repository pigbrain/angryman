-module(am_ws_handler).
-author("pigbrain").

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3]).

-define(HEALTHCHECK_INTERVAL, 5000).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    erlang:start_timer(?HEALTHCHECK_INTERVAL, self(), none),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    RequestCommand = jsx:decode(Msg, [return_maps]),
    CommandId = am_command:command_id(maps:get(<<"commandId">>, RequestCommand)),
    am_command:do(Req, State, CommandId, RequestCommand);

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(?HEALTHCHECK_INTERVAL, self(), Msg),
    am_command:post(Req, State, heartbeat_s2c, Msg);

websocket_info({broadcast, Msg}, Req, State) ->
    am_command:post(Req, State, player_sync_s2c, Msg);

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    am_world_watcher:unregister(self()),
    ok.
