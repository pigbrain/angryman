-module(am_world_watcher).
-author("pigbrain").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([register/2,
    unregister/1,
    broadcast/2,
    get_player_list/0]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(playerTable, [set, named_table]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register, Pid, PlayerState}, State) ->
    ets:insert(playerTable, {Pid, PlayerState}),
    {noreply, State};

handle_cast({unregister, Pid}, State) ->
    ets:delete(playerTable, Pid),
    {noreply, State};

handle_cast({broadcast, Pid, Message}, State) ->
    ets:insert(playerTable, {Pid, Message}),
    do_broadcast(Pid, Message, State);

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

register(Pid, PlayerState) ->
    gen_server:cast(?MODULE, {register, Pid, PlayerState}).

unregister(Pid) ->
    gen_server:cast(?MODULE, {unregister, Pid}).

broadcast(Pid, Message) ->
    gen_server:cast(?MODULE, {broadcast, Pid, Message}).

do_broadcast(Pid, Message, State) ->
    ets:safe_fixtable(playerTable, true),
    PlayerPid = ets:first(playerTable),
    try
        do_broadcast(PlayerPid, Pid, Message, State)
    after
        ets:safe_fixtable(playerTable, false)
    end.

do_broadcast('$end_of_table', _Pid, _Message, _State) ->
    {noreply, _State};
do_broadcast(PlayerPid, Pid, Message, State) ->
    case PlayerPid of
        Pid ->
            do_broadcast(ets:next(playerTable, PlayerPid), Pid, Message, State);
        _ ->
            PlayerPid ! {broadcast, Message},
            do_broadcast(ets:next(playerTable, PlayerPid), Pid, Message, State)
    end.

get_player_list() ->
    ets:safe_fixtable(playerTable, true),
    PlayerPid = ets:first(playerTable),
    try
        get_player_list(PlayerPid, [])
    after
        ets:safe_fixtable(playerTable, false)
    end.
get_player_list('$end_of_table', L) ->
    L;
get_player_list(PlayerPid, L) ->
    get_player_list(ets:next(playerTable, PlayerPid), [ets:lookup(playerTable, PlayerPid) | L]).