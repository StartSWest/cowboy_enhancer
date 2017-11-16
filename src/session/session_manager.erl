%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2016, Ivanco Software Corporation
%%% @doc
%%% This module manages the internal system sessions implementation
%%% and connection to the database.
%%% @end
%%% Created : 11. Jul 2015 4:00 AM
%%%-------------------------------------------------------------------
-module(session_manager).
-author("Ivan Carmenates Garcia").

-behaviour(gen_server).

%%-------------------------------------------------------------------------------------------------
%% API Exports
%%-------------------------------------------------------------------------------------------------
-export([
    start_link/0]).

-export([
    create_session/0,
    create_session/1,
    create_session/2,
    add_session_data/2,
    add_session_data_value/2,
    update_session_data/2,
    update_session_data_wrt/2,
    get_session/1,
    get_session/2,
    get_session_data/1,
    get_session_data/2,
    get_session_data_value/2,
%%     select_sessions/1,
%%     select_sessions/2,
    check_session/1,
    ensure_session/1,
    expire_session/1,
    refresh_session_exp_time/1,
    refresh_session_exp_time/2,
    delete_session/1,
    delete_session_data/2]).

-export([
    clean_expired_sessions/0,
    sgc_loop/1,
    get_server_name/0,
    get_ets_table_name/0]).

-export([
    test_all/0]).

%% gen_server callbacks
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
-define(SERVER, ce_session_manager).
-define(GARBAGE_COLLECTOR_NAME, ce_session_garbage_collector).

%% Name for the ETS session table in session manager.
-define(SESSION_ETS_TABLE_NAME, ce_session).

%% DO NOT MODIFY THIS: this is the fallback session expire time.
%% If you want to modify the value use 'app.config' file.
-define(SESSION_EXPIRE_TIME, timer:hours(1)).

%% DO NOT MODIFY THIS: this is the fallback GC frequency time.
%% If you want to modify the value use 'app.config' file.
-define(GARBAGE_COLLECTOR_FREQUENCY, timer:hours(1)).

%%-------------------------------------------------------------------------------------------------
%% RECORD Definition
%%-------------------------------------------------------------------------------------------------
-record(state, {
    gc_freq = ?GARBAGE_COLLECTOR_FREQUENCY :: integer(),
    session_exp_time = ?SESSION_EXPIRE_TIME :: integer()
}).

-record(session, {
    id = undefined :: session_id(),
    session_data = undefined :: session_data_map(),
    expire_time = undefined :: timeout(),
    expired_callback = undefined :: fun()
}).

%%-------------------------------------------------------------------------------------------------
%% TYPE Definition and Export
%%-------------------------------------------------------------------------------------------------
-type session_id() :: binary().
-type session_data_proplist() :: proplists:proplist().
-type session_data_map() :: map().
-type session_map() :: #{id => session_id(), session_data => session_data_map(), expire_time => timeout(), expired_callback => fun()}.
-type session_proplist() :: [{id, session_id()}, ...].

-export_type([
    session_id/0,
    session_proplist/0,
    session_map/0,
    session_data_map/0,
    session_data_proplist/0]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Creates a session with a generated unique id and an empty data.
%%
%% You can use `add_session_data/2' or `add_session_data_value/2'
%% functions to add data to it later.
%%
%% @see config_manager:session_manager_config/0.
%% @see create_session/2.
%% @see create_session/3.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec create_session() ->
    {ok, SessionID} when
    SessionID :: session_id().
create_session() ->
    gen_server:call(?SERVER,
        {create_session, #{}, undefined}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Creates a session with a generated unique id and a specified
%% `SessionDataMap' in map format.
%%
%% Example:
%% <pre>
%%   create_session(#{user_id => 1, username => "John"}).
%% </pre>
%% @see config_manager:session_manager_config/0.
%% @see create_session/2.
%% @see create_session/3.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec create_session(SessionDataMap) ->
    {ok, SessionID} when
    SessionDataMap :: map(),
    SessionID :: session_id().
create_session(SessionDataMap) when is_map(SessionDataMap) ->
    gen_server:call(?SERVER,
        {create_session, SessionDataMap, undefined}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Creates a session with a generated unique id and a specified
%% `SessionDataMap' in map format using the expired callback option.
%%
%% `ExpiredCallback' is a function that will be called once the
%% session expires and it must take an argument that is the expired
%% `Session'
%%
%% The return value of the expired callback controls the flow of the
%% expiration process as follow:<ul>
%% <li>
%%  `{cancel, default}': cancels the expiration process and refresh
%%  the expiration time with the default value set in the
%%  `app.config' file under `session_manager' tag.</li>
%% <li>
%%  `{cancel, NewExpirationTime}': does the same with the exception
%%  of a new expiration time will be used to refresh the expiration
%%  time of the session.</li>
%% <li>
%%  `any()' any other value will cause the session to be expired and
%%  deleted.</li></ul>
%% Example:
%% <pre>
%%   create_session(#{user_id => 1, username => "John"},
%%                  fun(ExpiredSession) -> {cancel, 1000} end).
%% </pre>
%% @see config_manager:session_manager_config/0.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec create_session(SessionDataMap, ExpiredCallback) ->
    {ok, SessionID} when
    SessionDataMap :: map(),
    ExpiredCallback :: fun(),
    SessionID :: session_id().
create_session(SessionDataMap, ExpiredCallback)
    when is_map(SessionDataMap) ->
    gen_server:call(?SERVER,
        {create_session, SessionDataMap, ExpiredCallback}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Adds new data in map format to the session data of a session
%% represented by a `SessionID' argument.
%%
%% Returns:
%%   `{error, {already_exist_data, ExistingDataMap}' if any key in
%%   `NewSessionDataMap' is already in session data.
%%
%% Example:
%% <pre>
%%   add_session_data(SessionID, #{age => 31, address => "Highway"}).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function will not reset the expiration time countdown of
%%  the session. You can use `refresh_session_expiration_time/1' after
%%  adding all data to reset session expiration time countdown.</li>
%% </ul>
%% @see add_session_data_value/2.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec add_session_data(SessionID, NewSessionDataMap) ->
    ok | {error, no_session} | {error, {already_exist_data, map()}} when
    SessionID :: session_id(),
    NewSessionDataMap :: map().
add_session_data(SessionID, NewSessionDataMap) when is_map(NewSessionDataMap) ->
    gen_server:call(?SERVER,
        {add_session_data, SessionID, NewSessionDataMap}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Adds a `{Key, Value}' pair of data to the session data of a session
%% represented by a `SessionID' argument.
%%
%% Returns:
%%   `{error, {already_exist_data, ExistingDataMap}' if any key in
%%   `NewSessionDataMap' is already in session data.
%%
%% Example:
%% <pre>
%%   add_session_data_value(SessionID, {age, 31}).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function will not reset the expiration time countdown of
%%  the session. You can use `refresh_session_expiration_time/1' after
%%  adding all data to reset session expiration time countdown.</li>
%% </ul>
%% @see add_session_data/2.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec add_session_data_value(SessionID, {Key, Value}) ->
    ok | {error, no_session} | {error, {already_exist_data, ExistingDataMap :: map()}} when
    SessionID :: session_id(),
    Key :: atom(),
    Value :: any().
add_session_data_value(SessionID, {Key, Value}) when is_atom(Key) ->
    gen_server:call(?SERVER,
        {add_session_data, SessionID, maps:from_list([{Key, Value}])}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Updates the session data of a session represented by a `SessionID'
%% argument.
%%
%% `NewSessionDataMap' must be a map with all already existing data
%% keys with new values. All nonexistent keys will be ignored.
%%
%% Example:
%% <pre>
%%   update_session_data(SessionID, #{username => "Tim"}).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function will also reset the expiration time of the session.
%%  If you don't want this default behavior to happen you can
%%  use `update_session_data_wrt/2' function.</li></ul>
%% @see update_session_data_wrt/2.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec update_session_data(SessionID, NewSessionDataMap) ->
    ok | {error, no_session} when
    SessionID :: session_id(),
    NewSessionDataMap :: map().
update_session_data(SessionID, NewSessionDataMap) when is_map(NewSessionDataMap) ->
    gen_server:call(?SERVER,
        {update_session_data, SessionID, NewSessionDataMap, refresh}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Updates the session data of a session represented by a `SessionID'
%% argument without resetting the expiration time of the session.
%%
%% `NewSessionDataMap' must be a map with all already existing data
%% keys with new values. All nonexistent keys will be ignored.
%%
%% Example:
%% <pre>
%%   update_session_data_wrt(SessionID,
%%       #{username => "Tim"}).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function will not reset the expiration time of the session.
%%  If you want the default behavior back simple use
%%  `update_session_data/2'.</li></ul>
%% @see update_session_data/2.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec update_session_data_wrt(SessionID, NewSessionDataMap) ->
    ok | {error, no_session} when
    SessionID :: session_id(),
    NewSessionDataMap :: map().
update_session_data_wrt(SessionID, NewSessionDataMap) when is_map(NewSessionDataMap) ->
    gen_server:call(?SERVER,
        {update_session_data, SessionID, NewSessionDataMap, no_refresh}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Refreshes the session expiration time of a session represented by
%% a `SessionID' argument.
%%
%% This function is equivalent to:
%% ```
%%   update_session_data(SessionID, #{}).
%% '''
%% @end
%%-------------------------------------------------------------------------------------------------
-spec refresh_session_exp_time(SessionID) ->
    ok | {error, no_session} when
    SessionID :: session_id().
refresh_session_exp_time(SessionID) ->
    gen_server:call(?SERVER,
        {refresh_session_exp_time, SessionID, default}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Refreshes the session expiration time of a session represented by
%% a `SessionID' argument by a given `ExpTime' in milliseconds.
%%
%% Example:
%% <pre>
%%   refresh_session_exp_time(SessionID, 1000). % 1 sec.
%% </pre>
%% Note:<ul>
%% <li>
%%  If `ExpTime' is `default' the default expiration time set in
%%  `app.config' file under the `session_manager' tag will be used.
%% </li></ul>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec refresh_session_exp_time(SessionID, ExpTime) ->
    ok | {error, no_session} when
    SessionID :: session_id(),
    ExpTime :: default | pos_integer().
refresh_session_exp_time(SessionID, ExpTime) ->
    gen_server:call(?SERVER,
        {refresh_session_exp_time, SessionID, ExpTime}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets the entire session object as a map by a `SessionID' argument.
%%
%% With this you can get the session expire time and expiration
%% callback function also the session data.
%%
%% This function is equivalent to:
%% ```
%%   get_session(SessionID, [{result_format, map}]),
%% '''
%% Example:
%% <pre>
%%   {ok, #{
%%        id := Id,
%%        session_data := SessionData,
%%        expired_callback := ExpiredCallback,
%%        expire_time := ExpireTime} = get_session(SessionID).
%% </pre>
%% Note:<ul>
%% <li>
%%  `SessionData' could be `no_data' if there is no session data.
%% </li></ul>
%% @see get_session/2.
%% @see get_session_data/1.
%% @see get_session_data/2.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_session(SessionID) ->
    {ok, Session} | {error, no_session} when
    SessionID :: session_id(),
    Session :: session_map().
get_session(SessionID) ->
    gen_server:call(?SERVER,
        {get_session, SessionID, [{result_format, map}]}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets the entire session object by a `SessionID' argument.
%%
%% With this you can get the session expire time and expiration
%% callback function also the session data.
%%
%% Example:
%% <pre>
%%   {ok, #{
%%        session_data := SessionData,
%%        expired_callback := ExpiredCallback,
%%        expire_time := ExpireTime,
%%        id := Id} = get_session(SessionID, [{result_format, map}]).
%% </pre>
%% Note:<ul>
%% <li>
%%  The return value depends on `result_format' option.
%%  `result_format' could be one of `proplist' or `map'.</li>
%% <li>
%%  `SessionData' could be `no_data' if there is no session data and
%%  `result_format' option is `map', or could be `[]' if is
%%  `proplist'.</li></ul>
%% @see get_session_data/1.
%% @see get_session_data/2.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_session(SessionID, Options) ->
    {ok, Session} | {error, no_session} when
    SessionID :: session_id(),
    Options :: {result_format, proplist | map} | [{result_format, proplist | map}],
    Session :: session_proplist() | session_map().
get_session(SessionID, Options) ->
    gen_server:call(?SERVER,
        {get_session, SessionID, Options}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets all values of the session data by a `SessionID' argument as
%% a map.
%%
%% This function is equivalent to:
%% ```
%%   get_session_data(SessionID, [{result_format, map}]).
%% '''
%% Notes:<ul>
%% <li>
%%  `SessionDataMap' could be `no_data' if there is no session data.
%% </li></ul>
%% @see get_session_data/2.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_session_data(SessionID) ->
    {ok, SessionDataMap | no_data} | {error, no_session} when
    SessionID :: session_id(),
    SessionDataMap :: session_data_map().
get_session_data(SessionID) ->
    gen_server:call(?SERVER,
        {get_session_data, SessionID, {result_format, map}}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets all values of the session data by a `SessionID' argument.
%%
%% Example:
%% <pre>
%%   get_session_data(SessionID, {result_format, map}).
%% </pre>
%% Note:<ul>
%% <li>
%%  The return value depends on `result_format' option.
%%  `result_format' could be one of `proplist' or `map'.</li>
%% <li>
%%  `SessionDataMap' could be `no_data' if there is no session data
%%  and `result_format' option is `map', or could be `[]' if is
%%  `proplist'.</li></ul>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_session_data(SessionID, Options) ->
    {ok, SessionData | no_data | []} | {error, no_session} when
    SessionID :: session_id(),
    Options :: {result_format, proplist | map} | [{result_format, proplist | map}],
    SessionData :: session_data_proplist() | session_data_map().
get_session_data(SessionID, Options) ->
    gen_server:call(?SERVER,
        {get_session_data, SessionID, Options}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets a specific value of the session data by a `SessionID' and
%% a `Key' argument.
%%
%% Example:
%% <pre>
%%   get_session_data_value(SessionID, age).
%% </pre>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_session_data_value(SessionID, Key) ->
    {ok, Data | undefined} | {error, no_session} when
    SessionID :: session_id(),
    Key :: atom(),
    Data :: any().
get_session_data_value(SessionID, Key) ->
    gen_server:call(?SERVER,
        {get_session_data_value, SessionID, Key}).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Checks whether a session exists or if is expired by its
%% `SessionID'. Also forces garbage collection on the session if is
%% already expired.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec check_session(SessionID) ->
    ok | {error, no_session} | {error, session_expired} when
    SessionID :: session_id().
check_session(SessionID) ->
    case get_session(SessionID) of
        {ok, Session} ->
            case check_session_expiration_time(Session) of
                ok ->
                    ok;
                {error, session_expired} ->
                    case expire_session_internal(Session) of
                        canceled ->
                            %% returns ok because the session is refreshed.
                            ok;
                        _ ->
                            {error, session_expired}
                    end
            end;
        Other ->
            Other
    end.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Does the same of `check_session/1' but also refreshes the
%% expiration countdown of the session.
%%
%% Note:<ul>
%% This function does 3 steps in one.
%% <li>
%%  Checks whether the session is not expired or doesn't exists.</li>
%% <li>
%%  Forces garbage collection if the session is already expired.</li>
%% <li>
%%  Refreshes the expiration countdown of the session, so the
%%  session will not expire while this function is called.</li></ul>
%% @see check_session/1.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec ensure_session(SessionID) ->
    ok | {error, no_session} | {error, session_expired} when
    SessionID :: session_id().
ensure_session(SessionID) ->
    case gen_server:call(?SERVER, {ensure_session, SessionID}, infinity) of
        {session_expired, Session} ->
            case expire_session_internal(Session) of
                canceled ->
                    %% returns ok because the session is refreshed.
                    ok;
                _ ->
                    {error, session_expired}
            end;
        Other ->
            Other
    end.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Removes data from the session data of a session represented by
%% `SessionID' and using a `Keys' key list argument.
%%
%% Example:
%% <pre>
%%   delete_session_data(SessionID, [username, age]).
%% </pre>
%% Note:<ul>
%% <li>
%%  Nonexistent keys will be ignored.</li>
%% <li>
%%  This function will not reset the expiration time countdown.
%%  You can use `refresh_session_expiration_time/1' after removing all
%%  data to reset session expiration time countdown.</li></ul>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec delete_session_data(SessionID, Keys) ->
    ok | {error, no_session} when
    SessionID :: session_id(),
    Keys :: [atom(), ...].
delete_session_data(SessionID, [K | _] = Keys) when is_atom(K), is_list(Keys) ->
    gen_server:call(?SERVER,
        {delete_session_data, SessionID, Keys}, infinity).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Deletes a session represented by a `SessionID' argument.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec delete_session(SessionID) ->
    ok | {error, no_session} when
    SessionID :: session_id().
delete_session(SessionID) ->
    gen_server:call(?SERVER,
        {delete_session, SessionID}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Cleans all the expired sessions.
%% @end
%%--------------------------------------------------------------------
-spec clean_expired_sessions() ->
    ok | {error, Reason} when
    Reason :: term().
clean_expired_sessions() ->
    case gen_server:call(?SERVER, get_expired_sessions, infinity) of
        [] ->
            ok;
        ExpiredSessions ->
            io:format("**** ExpiredSessions: ~p~n~n ***", [ExpiredSessions]),
            lists:foreach(fun(ExpiredSession) ->
                expire_session_internal(ExpiredSession)
            end, ExpiredSessions)
    end.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Does the expiration process of a session usually an expired one
%% by its `SessionID' executing its expiration callback and deleting
%% it if the callback does not cancel the process.
%%
%% The return value of the expired callback controls the flow of the
%% expiration process as follow:<ul>
%% <li>
%%  `{cancel, default}': cancels the expiration process and refresh
%%  the expiration time with the default value set in the
%%  `app.config' file under `session_manager' tag.</li>
%% <li>
%%  `{cancel, NewExpirationTime}': does the same with the exception
%%  of a new expiration time will be used to refresh the expiration
%%  time of the session.</li>
%% <li>
%%  `any()' any other value will cause the session to be expired and
%%  deleted.</li></ul>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec expire_session(SessionID) ->
    ok | {error, no_session} when
    SessionID :: session_id().
expire_session(SessionID) ->
    case get_session(SessionID, {result_format, map}) of
        {ok, Session} ->
            expire_session_internal(Session);
        Other ->
            Other
    end.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves all sessions that contains a specific session data map
%% information.
%%
%% This function is equivalent to:
%% ```
%%   select_sessions(#{role => "admin"}, {result_format, map}).
%% '''
%% Example:
%% <pre>
%%   {ok, Sessions} =
%%       select_sessions(#{role => "admin"}).
%% </pre>
%% Note:<ul>
%% <li>
%%  Each `Session' object is a map with the following structure.
%%  ```
%%    #{id => session_id(),
%%      session_data => session_map(),
%%      expire_time => timeout(),
%%      expired_callback => fun()}.
%% '''
%% </li></ul>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec select_sessions(SessionDataMapSpec) ->
    {ok, [Session, ...]} | {error, no_session} when
    Session :: session_map(),
    SessionDataMapSpec :: map().
select_sessions(SessionDataMapSpec) ->
    gen_server:call(?SERVER,
        {select_sessions, SessionDataMapSpec, [{result_format, map}]}, infinity).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves all sessions that contains a specific session data map
%% information using `Options'.
%%
%% Example:
%% <pre>
%%   {ok, Sessions} =
%%       select_sessions([{role, "admin"}], {result_format, map}).
%% </pre>
%% Note:<ul>
%% <li>
%%  Each `Session' object could be either a proplist or a map
%%  depending on `result_format' option, and have the following
%%  structure for `map' and a similar for `proplist':
%%  ```
%%    #{id => session_id(),
%%      session_data => session_proplist() | session_map(),
%%      expire_time => timeout(),
%%      expired_callback => fun()}.
%%  '''
%% </li>
%% <li>
%%  The `session_data' of each objects also follows the same
%%  `result_format' rule.
%% </li></ul>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec select_sessions(SessionDataMapSpec, Options) ->
    {ok, [Session, ...]} | {error, no_session} | {error, Other} when
    SessionDataMapSpec :: map(),
    Options :: {result_format, proplist | map} | [{result_format, proplist | map}],
    Session :: session_proplist() | session_map(),
    Other :: term().
select_sessions(SessionDataMapSpec, Options) ->
    gen_server:call(?SERVER,
        {select_sessions, SessionDataMapSpec, Options}, infinity).

%%%===================================================================
%%% Admin functions
%%%===================================================================

%% @private
get_server_name() ->
    ?SERVER.

%% @private
get_ets_table_name() ->
    ?SESSION_ETS_TABLE_NAME.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
        [{parent_process, self(), []}], [{timeout, infinity}]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([{parent_process, Pid, Data}]) ->
    case config_manager:session_manager_config() of
        {ok, Config} ->

            %%             %% TODO: Old code not using it!!!
            %%             mnesia:create_schema([node()]),
            %%             mnesia:start(),
            %%             mnesia:create_table(session, [
            %%                 {disc_only_copies, [node()]},
            %%                 {type, set},
            %%                 {attributes, record_info(fields, session)}]),
            %%
            %%             case mnesia:wait_for_tables([session], 5000) of
            %%                 {timeout, _RemainingTabs} ->
            %%                     {stop, mnesia_timeout};
            %%                 ok ->

            %% prepares the session config state.
            StateConfig = #state{
                session_exp_time = proplists:get_value(
                    session_expire_time, Config,
                    ?SESSION_EXPIRE_TIME),
                gc_freq = proplists:get_value(
                    garbage_collector_frequency, Config,
                    ?GARBAGE_COLLECTOR_FREQUENCY)},
            %% starts the session garbage collector.
            start_garbage_collector(StateConfig#state.gc_freq),
            {ok, StateConfig};
        %%             end;
        undefined ->
            {stop, invalid_or_missing_configuration}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call({create_session, SessionDataMap, ExpiredCallback}, _From, State) ->
    %% retrieves the session expire time.
    SessionExpTime = State#state.session_exp_time,
    %% creates the session.
    R = create_session_internal(SessionDataMap, SessionExpTime, ExpiredCallback),
    {reply, R, State};

handle_call({add_session_data, SessionID, NewSessionDataMap}, _From, State) ->
    R = case get_session_internal(SessionID) of
        {ok, Session} ->
            SessionDataMap = Session#session.session_data,
            NewSessionDataMapKeys = maps:keys(NewSessionDataMap),
            M = maps:with(NewSessionDataMapKeys, SessionDataMap),
            case maps:size(M) of
                0 ->
                    UpdatedSessionDataMap = maps:merge(SessionDataMap, NewSessionDataMap),
                    %% retrieves the session expiration time.
                    SessionExpTime = State#state.session_exp_time,
                    update_session_internal(
                        Session, UpdatedSessionDataMap, no_refresh, SessionExpTime);
                _ ->
                    {error, {already_exist_data, M}}
            end;
        Other ->
            Other
    end,
    {reply, R, State};

handle_call({update_session_data, SessionID, SessionDataMap, Mode}, _From, State) ->
    R = case get_session_internal(SessionID) of
        {ok, OldSession} ->
            OldSDataMap = OldSession#session.session_data,
            OldSDataKeys = maps:keys(OldSDataMap),
            NewSDataMap = maps:with(OldSDataKeys, SessionDataMap),
            UpdatedSessionDataMap = maps:merge(OldSDataMap, NewSDataMap),
            %% retrieves the session expiration time.
            SessionExpTime = State#state.session_exp_time,
            update_session_internal(
                OldSession, UpdatedSessionDataMap, Mode, SessionExpTime);
        Other ->
            Other
    end,
    {reply, R, State};

handle_call({get_session, SessionID, Options}, _From, State) ->
    R = case get_session_internal(SessionID) of
        {ok, Session} ->
            FixedOptions = case Options of
                Options when is_list(Options) -> Options; _ -> [Options]
            end,
            case proplists:get_value(result_format, FixedOptions) of
                map ->
                    {ok, record_to_map(Session)};
                proplist ->
                    {ok, record_to_proplist(Session)};
                undefined ->
                    {ok, record_to_map(Session)};
                Op ->
                    {error, {invalid_option, {result_format, Op}}}
            end;
        Other ->
            Other
    end,
    {reply, R, State};

handle_call({get_session_data, SessionID, Options}, _From, State) ->
    R = case get_session_internal(SessionID) of
        {ok, [Session]} ->
            FixedOptions = case Options of
                Options when is_list(Options) -> Options; _ -> [Options]
            end,
            case proplists:get_value(result_format, FixedOptions) of
                map ->
                    {ok, validate_session_data_map(Session#session.session_data)};
                proplist ->
                    {ok, maps:to_list(Session#session.session_data)};
                undefined ->
                    {ok, validate_session_data_map(Session#session.session_data)};
                Op ->
                    {error, {invalid_option, {result_format, Op}}}
            end;
        Other ->
            Other
    end,
    {reply, R, State};

handle_call({get_session_data_value, SessionID, Key}, _From, State) ->
    R = case get_session_internal(SessionID) of
        {ok, Session} ->
            {ok, maps:get(Key, Session#session.session_data, undefined)};
        Other ->
            Other
    end,
    {reply, R, State};

handle_call({delete_session, SessionID}, _From, State) ->
    ok = delete_session_internal(SessionID),
    {reply, ok, State};

handle_call({delete_session_data, SessionID, Keys}, _From, State) ->
    R = case get_session_internal(SessionID) of
        {ok, Session} ->
            SessionDataMap = Session#session.session_data,
            UpdatedSessionDataMap = maps:without(Keys, SessionDataMap),
            %% retrieves the session expiration time.
            SessionExpTime = State#state.session_exp_time,
            update_session_internal(
                Session, UpdatedSessionDataMap, no_refresh, SessionExpTime);
        Other ->
            Other
    end,
    {reply, R, State};

handle_call({ensure_session, SessionID}, _From, State) ->
    R = case get_session_internal(SessionID) of
        {ok, Session} ->
            case check_session_expiration_time(Session) of
                ok ->
                    ok = refresh_session_exp_time_internal(Session,
                        State#state.session_exp_time);
                {error, session_expired} ->
                    {session_expired, Session}
            end;
        Other ->
            Other
    end,
    {reply, R, State};

handle_call({refresh_session_exp_time, SessionID, ExpTime}, _From, State) ->
    R = case get_session_internal(SessionID) of
        {ok, Session} ->
            SessionExpTime = case ExpTime of
                default ->
                    State#state.session_exp_time;
                Other ->
                    Other
            end,
            refresh_session_exp_time_internal(Session, SessionExpTime);
        Other ->
            Other
    end,
    {reply, R, State};

handle_call(get_expired_sessions, _From, State) ->
    %% gets current time.
    Now = datetime_lib:now_integer(),
    MatchHead = #session{id = '$1', expire_time = '$2', _ = '_'},
    Guard = {'<', '$2', Now},
    Result = '$_',
    R = ets:select(?SESSION_ETS_TABLE_NAME, [{MatchHead, [Guard], [Result]}]),
    %%     R = mnesia:dirty_select(session, [{MatchHead, [Guard], [Result]}]),
    {reply, R, State};

handle_call({change_config, NewConfig}, _From, State) ->
    NewExpTime =
        proplists:get_value(session_expire_time, NewConfig, State#state.session_exp_time),
    NewGCFreq =
        proplists:get_value(garbage_collector_frequency, NewConfig, State#state.gc_freq),
    %% logs it.
    io:format(
        "~n=================================================~n"
        "  Session Manager configuration has changed to:"
        "~n-------------------------------------------------~n"
        "    session_expire_time: ~p~n"
        "    garbage_collector_frequency: ~p"
        "~n=================================================~n", [NewExpTime, NewGCFreq]),
    {reply, ok, State#state{session_exp_time = NewExpTime, gc_freq = NewGCFreq}}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a session with an unique id.
%% @end
%%--------------------------------------------------------------------
-spec create_session_internal(MapSessionDataMap, SessionExpireTime, ExpiredCallback) ->
    {ok, SessionID} when
    MapSessionDataMap :: map(),
    SessionExpireTime :: timeout(),
    ExpiredCallback :: fun(),
    SessionID :: session_id().
create_session_internal(MapSessionDataMap, SessionExpireTime, ExpiredCallback) ->
    %% The session does not exists so create it.
    %% generates an unique session id.
    SessionID = security_lib:generate_unique_id_hash(),
    Session = #session{
        id = SessionID,
        session_data = MapSessionDataMap,
        expire_time = datetime_lib:now_integer_timeout(SessionExpireTime),
        expired_callback = ExpiredCallback},
    %% stores the session in the session database.
    ok = write_session_internal(Session),
    {ok, SessionID}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gets a session by its `SessionID'.
%% @end
%%--------------------------------------------------------------------
-spec get_session_internal(SessionID) ->
    {ok, Session} | {error, no_session} when
    SessionID :: session_id(),
    Session :: erlang:record(session).
get_session_internal(SessionID) ->
    case ets:lookup(?SESSION_ETS_TABLE_NAME, SessionID) of
        %%     case mnesia:dirty_read({session, SessionID}) of
        [] ->
            {error, no_session};
        [Session] when is_record(Session, session) ->
            {ok, Session}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a session with `UpdatedSessionDataMap'.
%% @end
%%--------------------------------------------------------------------
-spec update_session_internal(OldSession, UpdatedSessionDataMap, Mode, SessionExpireTime) ->
    ok when
    OldSession :: erlang:record(session),
    UpdatedSessionDataMap :: map(),
    Mode :: refresh | no_refresh,
    SessionExpireTime :: timeout().
update_session_internal(OldSession, UpdatedSessionDataMap, Mode, SessionExpireTime)
    when is_record(OldSession, session) ->
    NewSession = OldSession#session{
        session_data = UpdatedSessionDataMap,
        expire_time = case Mode of
            no_refresh ->
                OldSession#session.expire_time;
            refresh ->
                datetime_lib:now_integer_timeout(SessionExpireTime)
        end
    },
    ok = write_session_internal(NewSession).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Refreshes a session expiration time.
%% @end
%%--------------------------------------------------------------------
-spec refresh_session_exp_time_internal(Session, SessionExpireTime) -> ok when
    Session :: erlang:record(session),
    SessionExpireTime :: timeout().
refresh_session_exp_time_internal(Session, SessionExpireTime)
    when is_record(Session, session) ->
    NewSession = Session#session{
        expire_time = datetime_lib:now_integer_timeout(SessionExpireTime)
    },
    ok = write_session_internal(NewSession).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds all sessions that contains the specified session data.
%% @end
%%--------------------------------------------------------------------
-spec find_sessions_by_data(SessionDataMap, Limit) ->
    {ok, SessionID | [SessionID, ...]} | {error, no_session} when
    SessionDataMap :: session_data_map(),
    Limit :: pos_integer(),
    SessionID :: session_id().
find_sessions_by_data(SessionDataMap, Limit) ->
    MatchHead = #session{id = '$1', session_data = '$2', _ = '_'},
    Guard = {'==', '$2', SessionDataMap},
    Result = '$1',
    case ets:select(?SESSION_ETS_TABLE_NAME, [{MatchHead, [Guard], [Result]}], Limit) of
        [] ->
            {error, no_session};
        SessionIDs ->
            {ok, SessionIDs}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores a session data to the session database.
%% @end
%%--------------------------------------------------------------------
-spec write_session_internal(Session) -> ok when
    Session :: erlang:record(session).
write_session_internal(Session) ->
    %%     mnesia:dirty_write(Session),
    true = ets:insert(?SESSION_ETS_TABLE_NAME, Session),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes a session.
%% @end
%%--------------------------------------------------------------------
-spec delete_session_internal(SessionID) -> ok when
    SessionID :: session_id().
delete_session_internal(SessionID) ->
    %%     mnesia:delete({session, SessionID}),
    true = ets:delete(?SESSION_ETS_TABLE_NAME, SessionID),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks whether a session has expired.
%% @end
%%--------------------------------------------------------------------
-spec check_session_expiration_time(Session) ->
    ok | {error, session_expired} when
    Session :: erlang:record(session).
check_session_expiration_time(Session) ->
    Now = datetime_lib:now_integer(),
    case Now > Session#session.expire_time of
        true ->
            {error, session_expired};
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to expire a specified session.
%%
%% NOTE: If `canceled' is returned the session will not be expired
%%       and its expiration time will be refreshed.
%%       If `ok | {ok, _} | {error, _}' is returned that means that
%%       the session was deleted. The error could be produced in
%%       the expiration callback. However the session will be deleted
%%       anyway.
%% @end
%%--------------------------------------------------------------------
-spec expire_session_internal(ExpiredSession) ->
    ok | {ok, CallBackReturnValue}| {error, Reason} | canceled when
    ExpiredSession :: erlang:record(session),
    CallBackReturnValue :: any(),
    Reason :: term().
expire_session_internal(ExpiredSession) ->
    %% checks if the session has a callback function.
    F = ExpiredSession#session.expired_callback,
    %% gets the result of the expired callback function
    ER = (catch case ((F /= undefined) and (is_function(F))) of
        true ->
            %% Calls the expired callback function of the session.
            F(record_to_map(ExpiredSession));
        _ ->
            ok
    end),
    case ER of
        {cancel, default} ->
            refresh_session_exp_time(ExpiredSession, default),
            canceled;
        {cancel, NewExpireTime} when is_integer(NewExpireTime) ->
            refresh_session_exp_time(ExpiredSession, NewExpireTime),
            canceled;
        {'EXIT', Reason} ->
            %% ensures that the session is deleted anyway if an error
            %% occurs inside the expiration function.
            delete_session(ExpiredSession#session.id),
            %% logs the error.
            debug_logger:log_error_msg(Reason),
            {error, Reason};
        CallBackReturnValue ->
            delete_session(ExpiredSession#session.id),
            {ok, CallBackReturnValue}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts session record to a map.
%% @end
%%--------------------------------------------------------------------
-spec record_to_map(Record) -> map() when
    Record :: erlang:record(session).
record_to_map(Record) ->
    #{
        id => Record#session.id,
        session_data => validate_session_data_map(Record#session.session_data),
        expire_time => Record#session.expire_time,
        expired_callback => Record#session.expired_callback
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns `no_data' if `SessionDataMap' is a zero size map or the
%% entire map otherwise.
%% @end
%%--------------------------------------------------------------------
-spec validate_session_data_map(SessionDataMap) ->
    no_data | SessionDataMap when
    SessionDataMap :: map().
validate_session_data_map(SessionDataMap) ->
    case maps:size(SessionDataMap) of
        0 -> no_data;
        _ -> SessionDataMap
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts session record to a proplist.
%% @end
%%--------------------------------------------------------------------
-spec record_to_proplist(Record) -> proplists:proplist() when
    Record :: erlang:record(session).
record_to_proplist(Record) ->
    [
        {id, Record#session.id},
        {session_data, maps:to_list(Record#session.session_data)},
        {expire_time, Record#session.expire_time},
        {expired_callback, Record#session.expired_callback}
    ].

%%-------------------------------------------------------------------------------------------------
%% Garbage Collector functions
%%-------------------------------------------------------------------------------------------------

%% @private
start_garbage_collector(GCFreq) ->
    %% registers the GC as a named process.
    register(?GARBAGE_COLLECTOR_NAME,
        spawn_link(?MODULE, sgc_loop, [GCFreq])),
    {ok, self()}.

%% @private
sgc_loop(Interval) ->
    receive after Interval ->
        session_manager:clean_expired_sessions(),
        sgc_loop(Interval)
    end.

%%%-------------------------------------------------------------------
%%% Test Functions
%%%-------------------------------------------------------------------

test_all() ->
    io:format("*** Starting Tests ***~n~n"),
    io:format("*** PLEASE Configure the session expiration time to 5 secounds before continue ***~n~n"),
    timer:sleep(1000),

    io:format("1. Creating an empty session.~n"), timer:sleep(1000),
    {ok, SessionId} = create_session(),
    io:format("   done: SessionId: ~p.~n~n", [SessionId]), timer:sleep(1000),

    io:format("2. Creating an session with #{id=>1, name=>'John'}.~n"), timer:sleep(1000),
    {ok, SessionId2} = create_session(#{id=>1, name=>'John'}),
    io:format("   done: SessionId: ~p.~n~n", [SessionId2]), timer:sleep(1000),

    io:format("3. Creating an session with #{id=>1, name=>'John'} and an expired callback"
    "to trigger the message 'This is nice!!!'.~n"), timer:sleep(1000),
    {ok, SessionId3} = create_session(#{id=>2, name=>'Tim'}, fun(_ID) ->
        io:format("3. Expired Callback: --- This is nice!!! session expired ---~n~n") end),
    io:format("   done: SessionId: ~p.~n~n", [SessionId3]), timer:sleep(8000),

    io:format("4. Creating an session with an expired callback to trigger the message"
    "'Canceled, default' and canceling the expiration process.~n"), timer:sleep(1000),
    {ok, SessionId4} = create_session(#{id=>4, name=>'Tim'},
        fun(ExpiredSession4) ->
            #{id:=SID, session_data:=Data} = ExpiredSession4,
            #{id:=ID} = Data,
            case ID of
                4 ->
                    session_manager:update_session_data_wrt(SID, #{id => 0}),
                    io:format("4. Expired Callback: --- Canceled, default ---~n~n"),
                    {cancel, default};
                _ ->
                    io:format("4. Expired Callback: --- Finished after default time ---~n~n"),
                    ok
            end end),
    io:format("   done: SessionId: ~p.~n~n", [SessionId4]), timer:sleep(12000),

    io:format("5. Creating an session with an expired callback to trigger the message"
    "'Canceled, 1 sec' and canceling the expiration process.~n"), timer:sleep(1000),
    {ok, SessionId5} = create_session(#{id=>5, name=>'Tim'},
        fun(ExpiredSession5) ->
            #{id:=SID, session_data:=Data} = ExpiredSession5,
            #{id:=ID} = Data,
            case ID of
                5 ->
                    session_manager:update_session_data_wrt(SID, #{id => 0}),
                    io:format("5. Expired Callback: --- Canceled, 1 second ---~n~n"),
                    {cancel, 1000};
                _ ->
                    io:format("5. Expired Callback: --- Finished after 1 sec---~n~n"),
                    ok
            end end),
    io:format("   done: SessionId: ~p.~n~n", [SessionId5]), timer:sleep(10000),

    io:format("*** DONE ALL TESTS ***~n").

%%     create_session / 0,
%%     create_session / 1,
%%     create_session / 2,
%%     add_session_data / 2,
%%     add_session_data_value / 2,
%%     update_session_data / 2,
%%     update_session_data_wrt / 2,
%%     get_session / 1,
%%     get_session / 2,
%%     get_session_data / 1,
%%     get_session_data / 2,
%%     get_session_data_value / 2,
%%     find_session / 1,
%%     find_session / 2,
%%     find_sessions / 1,
%%     find_sessions / 2,
%%     check_session / 1,
%%     expire_session / 1,
%%     refresh_session_expiration_time / 1,
%%     delete_session / 1,
%%     delete_session_data / 2]).