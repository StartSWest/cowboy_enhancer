%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% This the connection multi pool gen_server for database manager.
%%% So you can start many pools, i.e. one for each database backend.
%%% @end
%%% Created : 14. Aug 2015 8:48 PM
%%%-------------------------------------------------------------------
-module(connection_pool).
-author("Ivan Carmenates Garcia").

%% Admin API Exports
-export([
    start_link/2]).

%% gen_server Callbacks Exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%%-------------------------------------------------------------------------------------------------
%% MACRO Definition
%%-------------------------------------------------------------------------------------------------
-define(CONFIG_OPTIONS, [
    backend,
    server,
    username,
    password,
    database,
    %% max number of connections in the pool.
    max_reusable_connections,
    %% time to wait for a available connection.
    wait_for_reusable_connection_timeout
]).

%%-------------------------------------------------------------------------------------------------
%% Admin API Functions
%%-------------------------------------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Starts the connection pool for a backend.
%% @end
%%--------------------------------------------------------------------
-spec start_link(PoolName, BackendConfig) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()} when
    PoolName :: atom(),
    BackendConfig :: proplists:proplist().
start_link(PoolName, BackendConfig) ->
    gen_server:start_link({local, PoolName}, ?MODULE, BackendConfig, [{timeout, infinity}]).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

-spec init(BackendConfig) ->
    {ok, State} | {ok, State, timeout() | hibernate} | {stop, Reason} | ignore when
    BackendConfig :: proplists:proplist(),
    State :: map(),
    Reason :: term().
init(BackendConfig) ->
    %% checks if all the config options are valid and all present.
    %% See start_link/2 edoc for Config struct.
    case validate_options(BackendConfig) of
        true ->
            %% prepares the 'State' map with all necessary data.
            PartState = #{
                connection_pool => queue:new(),
                caller_pool => queue:new(),
                connection_pool_current_size => 0},
            State = maps:merge(PartState, maps:from_list(BackendConfig)),
            {ok, State};
        false ->
            {stop, {invalid_config, BackendConfig}}
    end.

handle_call({update_options, NewOptions}, _From, State) ->
    ValidOptions = lists:filter(fun({OptionName, _}) ->
        lists:member(OptionName, ?CONFIG_OPTIONS)
    end, NewOptions),
    case lists:subtract(NewOptions, ValidOptions) of
        [] ->
            %% if there is no unknown options in 'NewOptions'
            NewState =
                lists:foldl(fun({Name, Value}, SM) ->
                    maps:update(Name, Value, SM)
                end, State, NewOptions),
            {reply, ok, NewState};
        InvalidOptions ->
            {reply, {error, {invalid_options, InvalidOptions}}, State}
    end;
handle_call({connect, Timeout}, _From, State) ->
    #{backend := Backend,
        server := Server,
        username := Username,
        password := Password,
        database := Database,
        wait_for_reusable_connection_timeout := DefaultTimeout} = State,
    Timeout2 = case Timeout of
        default_timeout -> DefaultTimeout; _ -> Timeout
    end,
    R = Backend:connect(Server, Username, Password, Database, Timeout2),
    %% returns {R, Backend}, R could be {ok, Connection} or {error, ...}.
    {reply, {R, Backend}, State};
handle_call({disconnect, Connection}, _From, State) ->
    #{backend := Backend} = State,
    R = Backend:disconnect(Connection),
    {reply, R, State};
handle_call({get_connection, Timeout}, _From, State) ->
    #{connection_pool_current_size := CurrentCount, connection_pool := Pool} = State,
    %% tries to pop a connection from the pool.
    case queue:out(Pool) of
        {empty, _} ->
            %% if no connections in the queue. Checks if there is still possible
            %% to create a new one.
            #{max_reusable_connections := MaxReusableConnections} = State,
            case CurrentCount < MaxReusableConnections of
                true ->
                    %% creates a new connection
                    case handle_call({connect, Timeout}, _From, State) of
                        {reply, {{ok, Connection}, Backend}, _} ->
                            %% reply to the caller with the new connection just created.
                            NewState = State#{
                                connection_pool_current_size => CurrentCount + 1},
                            %% DBSession is created.
                            {reply, {ok, {Backend, Connection}}, NewState};
                        _ ->
                            #{wait_for_reusable_connection_timeout := DWTimeout} = State,
                            %% in case an error occurs when trying to create
                            %% the connection.
                            %% NOTE: the last parameter 'DWTimeout' setups the caller
                            %% when no connection available to wait for a specific time
                            %% for a connection to be available.
                            {reply, {error, no_available_connection, DWTimeout}, State}
                    end;
                false ->
                    #{wait_for_reusable_connection_timeout := DWTimeout} = State,
                    %% if the limit is reached and no connection can be created.
                    %% NOTE: the last parameter 'DWTimeout' setups the caller
                    %% when no connection available to wait for a specific time
                    %% for a connection to be available.
                    {reply, {error, no_available_connection, DWTimeout}, State}
            end;
        {{value, Connection}, NewConnectionPool} ->
            %% there is an available connection in the queue.
            NewState = State#{connection_pool => NewConnectionPool},
            %% gets the backend to be returned.
            #{backend := Backend} = NewState,
            %% DBSession is reused.
            {reply, {ok, {Backend, Connection}}, NewState}
    end;
handle_call({register_caller, Pid}, _From, State) ->
    %% inserts a new caller in the caller queue.
    #{caller_pool := CallerPool} = State,
    NewCallerPool = queue:in(Pid, CallerPool),
    NewState = State#{caller_pool => NewCallerPool},
    {reply, ok, NewState};
handle_call({unregister_caller, Pid}, _From, State) ->
    %% removes the specified caller from the caller queue.
    #{caller_pool := CallerPool} = State,
    NewCallerPool = queue:filter(fun(X) -> Pid =/= X end, CallerPool),
    NewState = State#{caller_pool => NewCallerPool},
    {reply, ok, NewState};
handle_call({free_connection, Connection}, _From, State) ->
    %% frees the connection to be used by another caller.
    %% NOTE: this also unregisters the caller if there is any waiting
    %%       for a connection.
    #{caller_pool := CallerPool} = State,
    {Reply, PreState} =
        case queue:out(CallerPool) of
            {empty, _} ->
                %% no callers awaiting for available connections
                {ok, State};
            {{value, ReqPid}, NewCallerPool} when is_pid(ReqPid) ->
                %% there is a caller waiting for an available connection.
                %% updates the caller pool with the caller removed.
                NewState = State#{caller_pool => NewCallerPool},
                %% informs the caller to claim a connection again.
                {{ask_connection, ReqPid}, NewState}
        end,
    #{connection_pool_current_size := CurrentCount,
        max_reusable_connections := MaxCount} = PreState,
    %% checks if the connection pool was reduced.
    case (CurrentCount > MaxCount) of
        true ->
            %% new available connection now is discarded and disconnected from
            %% the database, because the connection pool was reduced.
            #{backend := Backend} = PreState,
            Backend:disconnect(Connection),
            NewState2 = PreState#{connection_pool_current_size => CurrentCount - 1},
            {reply, Reply, NewState2};
        false ->
            %% in case the connection pool remains the same, put back in the
            %% released connection into the pool.
            #{connection_pool := ConnectionPool} = PreState,
            NewConnectionPool = queue:in(Connection, ConnectionPool),
            NewState2 = PreState#{connection_pool => NewConnectionPool},
            {reply, Reply, NewState2}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

validate_options(Options) ->
    Keys = proplists:get_keys(Options),
    %% checks that 'Options' has exactly all '?CONFIG_OPTIONS'.
    lists:subtract(Keys, ?CONFIG_OPTIONS) == lists:subtract(?CONFIG_OPTIONS, Keys).