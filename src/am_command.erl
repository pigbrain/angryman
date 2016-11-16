-module(am_command).
-author("pigbrain").

-include("am_record.hrl").

%% API
-export([result/1,
    command_id/1,
    do/4,
    post/4]).

result(Result) ->
    case Result of
        success -> "200"
    end.

command_id(Command) ->
    case Command of
        login_c2s -> "100000";
        login_s2c -> "100001";
        move_c2s -> "100010";
        move_s2c -> "100011";
        stop_c2s -> "100012";
        stop_s2c -> "100013";
        player_sync_c2s -> "400001";
        player_sync_s2c -> "400002";
        field_sync_c2s -> "400003";
        field_sync_s2c -> "400004";
        heartbeat_c2s -> "500000";
        heartbeat_s2c -> "500001";
        <<"100000">> -> login_c2s;
        <<"100001">> -> login_s2c;
        <<"100010">> -> move_c2s;
        <<"100011">> -> move_s2c;
        <<"100012">> -> stop_c2s;
        <<"100013">> -> stop_s2c;
        <<"400001">> -> player_sync_c2s;
        <<"400002">> -> palyer_sync_s2c;
        <<"400003">> -> field_sync_c2s;
        <<"400004">> -> field_sync_s2c;
        <<"500000">> -> heartbeat_c2s;
        <<"500001">> -> heartbeat_s2c;
        _ -> undefined
    end.

%% LOGIN
do(Req, _State, login_c2s, RequestCommand) ->
    Character = maps:get(<<"character">>, RequestCommand),
    Nickname = maps:get(<<"nickname">>, RequestCommand),


    Result = am_command:result(success),
    CommandId = am_command:command_id(login_s2c),
    PlayerId = erlang:phash2({node(), now()}), %erlang:unique_integer(),
    PlayerState = #player{playerId = PlayerId, character = Character, nickname = Nickname, state = login},

    am_world_watcher:register(self(), PlayerState),
    am_world_watcher:broadcast(self(), PlayerState),

    {reply, {text, jsx:encode([{<<"result">>, list_to_binary(Result)},
        {<<"commandId">>, list_to_binary(CommandId)},
        {<<"character">>, PlayerState#player.character},
        {<<"playerId">>, PlayerState#player.playerId},
        {<<"positionX">>, PlayerState#player.positionX},
        {<<"positionY">>, PlayerState#player.positionY}])}, Req, PlayerState};

%% MOVE
do(Req, State, move_c2s, RequestCommand) ->
    CommandId = am_command:command_id(move_s2c),

    doMove(Req, State, CommandId, move, RequestCommand);

%% STOP
do(Req, State, stop_c2s, RequestCommand) ->
    CommandId = am_command:command_id(stop_s2c),

    doMove(Req, State, CommandId, stop, RequestCommand);

%% SYNC FIELD
do(Req, State, field_sync_c2s, _RequestCommand) ->

    Result = am_command:result(success),
    CommandId = am_command:command_id(field_sync_s2c),

    PlayerList = am_world_watcher:get_player_list(),

    PlayerList2 = lists:map(fun(P) ->
                                T = list_to_tuple(P),
                                {{_,  Player}} =  T,
                                jsx:encode([
                                    {<<"playerId">>, Player#player.playerId},
                                    {<<"character">>, Player#player.character},
                                    {<<"nickname">>, Player#player.nickname},
                                    {<<"directionX">>, Player#player.directionX},
                                    {<<"directionY">>, Player#player.directionY},
                                    {<<"positionX">>, Player#player.positionX},
                                    {<<"positionY">>, Player#player.positionY},
                                    {<<"state">>, atom_to_binary(Player#player.state, utf8)}])
                            end, PlayerList),

    {reply, {text, jsx:encode([{<<"result">>, list_to_binary(Result)},
        {<<"commandId">>, list_to_binary(CommandId)},
        {<<"playerList">>, PlayerList2}])},
        Req,
        State}.


post(Req, State, heartbeat_s2c, _Msg) ->
    Result = am_command:result(success),
    CommandId = am_command:command_id(heartbeat_s2c),
    {reply, {text, jsx:encode([{<<"result">>, list_to_binary(Result)},
        {<<"commandId">>, list_to_binary(CommandId)}])}, Req, State};

post(Req, State, player_sync_s2c, Msg) ->

    Result = am_command:result(success),
    CommandId = am_command:command_id(player_sync_s2c),

    {reply, {text, jsx:encode([{<<"result">>, list_to_binary(Result)},
        {<<"commandId">>, list_to_binary(CommandId)},
        {<<"playerId">>, Msg#player.playerId},
        {<<"character">>, Msg#player.character},
        {<<"nickname">>, Msg#player.nickname},
        {<<"directionX">>, Msg#player.directionX},
        {<<"directionY">>, Msg#player.directionY},
        {<<"positionX">>, Msg#player.positionX},
        {<<"positionY">>, Msg#player.positionY},
        {<<"state">>, atom_to_binary(Msg#player.state, utf8)}])}, Req, State}.

doMove(Req, State, CommandId, Act, RequestCommand) ->
    DirectionX = maps:get(<<"directionX">>, RequestCommand),
    DirectionY = maps:get(<<"directionY">>, RequestCommand),
    PositionX = maps:get(<<"positionX">>, RequestCommand),
    PositionY = maps:get(<<"positionY">>, RequestCommand),

    Result = am_command:result(success),

    PlayerState = State#player{directionX = DirectionX, directionY = DirectionY, positionX = PositionX, positionY = PositionY, state = Act},

    am_world_watcher:broadcast(self(), PlayerState),

    {reply, {text, jsx:encode([{<<"result">>, list_to_binary(Result)},
        {<<"commandId">>, list_to_binary(CommandId)},
        {<<"directionX">>, PlayerState#player.directionX},
        {<<"directionY">>, PlayerState#player.directionY},
        {<<"positionX">>, PlayerState#player.positionX},
        {<<"positionY">>, PlayerState#player.positionY}])},
        Req,
        PlayerState}.