%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% This module manages the direct actions over the database backends,
%%% it is also a generic interface to it.
%%% @end
%%% Created : 13. Jul 2015 3:52 PM
%%%-------------------------------------------------------------------
-module(database_manager).
-author("Ivan Carmenates Garcia").

-behaviour(gen_server).

%% -------------------------------------------------------------------
%% API Exports
%% -------------------------------------------------------------------
-export([
    connect/1,
    connect/2,
    connection_block/1,
    connection_block/2,
    connection_block_transaction/1,
    connection_block_transaction/2,
    disconnect/1,
    transaction/2,
    query/3,
    query/4,
    insert/2,
    insert/3,
    update/3,
    update/4,
    delete/3,
    delete/4,
    find/3,
    find/4,
    find_one/3,
    find_one/4]).

%% -------------------------------------------------------------------
%% Admin API Exports
%% -------------------------------------------------------------------
-export([
    install_backend/2,
    remove_backend/1,
    update_backend_options/2,
    start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% -------------------------------------------------------------------
%% MACRO Definition
%% -------------------------------------------------------------------
-define(SERVER, ce_database_manager).
-define(CONNECTION_POOL_SUP_NAME, ce_connection_pool_sup).

%% fail-default max amount of connections in the connection pool.
-define(MAX_REUSABLE_CONNECTIONS, 10).
%% fail-default max time in milliseconds to wait for an available
%% connection.
-define(WAIT_FOR_REUSABLE_CONNECTION_TIMEOUT, 5000).

%% -------------------------------------------------------------------
%% TYPE Definition
%% -------------------------------------------------------------------
-type db_session() :: {Backend :: atom(), Connection :: any()}.

-type map_result_data() :: no_data | map() | [map(), ...].
-type proplist_result_data() :: [] | [proplists:proplist(), ...].
-type raw_result_data() :: any().
-type result_data() :: map_result_data() | proplist_result_data() | raw_result_data().
-type result_data_for_insert() :: map() | [proplists:proplist()] | any().

-type table_name() :: atom().
-type field_name() :: atom().
-type full_field_name() :: {table_name(), field_name()} | {table_name(), field_name(), field_name_alias()}.
-type field_name_alias() :: atom().
-type field_value() :: term().

-type simple_return_field_specs() :: [field_name(), ...].
-type full_return_field_specs() :: [field_name() | {table_name(), field_name()} | {table_name(), [field_name(), ...]}, ...].

-type operator() :: gt | '>' | lt | '<' | gte | '>=' | lte | '=<' | 'eq' | '==' | ne | '/=' | 'like'.

-type condition_term() :: {field_name() | full_field_name() | field_value(), operator(), field_name() | full_field_name() | field_value()}.
-type logic_operator() :: 'and' | 'or'.

-type order_by_expr() :: [field_value() | full_field_name() | {field_name() | full_field_name(), asc | desc}, ...].
-type group_by_expr() :: [field_value() | full_field_name(), ...].

-type match_field_specs() :: [condition_term(), ...] | list() | map().

-type table_spec() :: {table_name(), map()}.

-type query_params() :: [any(), ...] | [].

-export_type([db_session/0, map_result_data/0, proplist_result_data/0, raw_result_data/0,
    table_name/0, field_name/0, full_field_name/0, field_value/0, operator/0, condition_term/0,
    logic_operator/0, match_field_specs/0, table_spec/0, query_params/0,
    simple_return_field_specs/0, full_return_field_specs/0, result_data/0,
    result_data_for_insert/0, order_by_expr/0, group_by_expr/0]).

%% -------------------------------------------------------------------
%% Record Definition
%% -------------------------------------------------------------------
-record(state, {}).

-record(backend_config, {
    backend :: atom(),
    server :: string(),
    username :: string(),
    password :: string(),
    database :: string(),
    max_reusable_connections :: integer(),
    wait_for_reusable_connection_timeout :: timeout()
}).

%% -------------------------------------------------------------------
%% @doc
%% Tries to connect to a database backend. Returns a `DBSession'
%% with information about the backend and connection.
%%
%% Warning: using this function is not recommended unless you know
%% what you are doing, because it will create a connection outside of
%% the connection pool. Use `connection_block/1/2' instead to get an
%% available connection from the pool.
%%
%% Note:<ul>
%% <li>
%%   `DBSession' is used by many of the API functions.</li>
%% <li>
%%   It uses the backend `wait_for_reusable_connection_timeout'
%%   option as a connection timeout.</li>
%% <li>
%%   The backend is configured in `cowboy_enhancer' environment
%%   variable called `database_manager'. i.e.:</li></ul>
%% <pre>
%%    {database_manager, [
%%        {main_backend, [
%%            {backend, postgres_backend},
%%            {server, "localhost"},
%%            {username, "postgres"},
%%            {password, "server"},
%%            {database, "eoc_db"},
%%            {max_reusable_connections, 10}, % 10 connections.
%%            {wait_for_reusable_connection_timeout, 5000} % 5 sec.
%%        ]}
%%    ]}
%% </pre>
%% @see connect/2.
%% @end
%% -------------------------------------------------------------------
-spec connect(BackendName) ->
    {ok, DBSession}
    | {error, {invalid_backend, BackendName}}
    | {error, Other} when
    BackendName :: atom(),
    DBSession :: db_session(),
    Other :: term().
connect(BackendName) ->
    connect(BackendName, default_timeout).

%% -------------------------------------------------------------------
%% @doc
%% Tries to connect to a database backend within a `Timeout'.
%% Returns a `DBSession' with information about the backend and
%% connection.
%%
%% Warning: using this function is not recommended unless you know
%% what you are doing, because it will create a connection outside of
%% the connection pool. Use `connection_block/1/2' instead to get an
%% available connection from the pool.
%%
%% Note:<ul>
%% <li>
%%   `DBSession' is used by many of the API functions.</li>
%% <li>
%%   It uses a `Timeout' in milliseconds to wait for connection. It
%%   overrides `wait_for_reusable_connection_timeout' backend
%%   option.</li>
%% <li>
%%   You can set `Timeout = default_timeout' to use the backend's
%%   `wait_for_reusable_connection_timeout' option.</li>
%% <li>
%%   The backend is configured in `cowboy_enhancer' environment
%%   variable called `database_manager'. i.e.:</li></ul>
%% <pre>
%%    {database_manager, [
%%        {main_backend, [
%%            {backend, postgres_backend},
%%            {server, "localhost"},
%%            {username, "postgres"},
%%            {password, "server"},
%%            {database, "eoc_db"},
%%            {max_reusable_connections, 10}, % 10 connections.
%%            {wait_for_reusable_connection_timeout, 5000} % 5 sec.
%%        ]}
%%    ]}
%% </pre>
%% @end
%% -------------------------------------------------------------------
-spec connect(BackendName, Timeout) ->
    {ok, DBSession}
    | {error, {invalid_backend, BackendName}}
    | {error, Other} when
    BackendName :: atom(),
    Timeout :: timeout() | default_timeout,
    DBSession :: db_session(),
    Other :: term().
connect(BackendName, Timeout) ->
    case check_backend_connection_pool(BackendName) of
        {ok, ConnectionPool} ->
            case gen_server:call(ConnectionPool, {connect, Timeout}) of
                {{ok, Connection}, Backend} ->
                    %% creates a 'DBSession' to be used by other API functions.
                    DBSession = {Backend, Connection},
                    {ok, DBSession};
                Other ->
                    Other
            end;
        Other2 ->
            Other2
    end.

%% -------------------------------------------------------------------
%% @doc
%% Disconnects from the specified `DBSession'.
%% @end
%% -------------------------------------------------------------------
-spec disconnect(DBSession) -> ok when
    DBSession :: db_session().
disconnect({Backend, Connection}) ->
    case check_backend_connection_pool(Backend) of
        {ok, ConnectionPool} ->
            %% closes the connection related to 'DBSession'.
            gen_server:call(ConnectionPool, {disconnect, Connection});
        _ ->
            %% if no backend no connection.
            ok
    end.

%% -------------------------------------------------------------------
%% @doc
%% Tries to create a connection session with an available connection
%% from the connection pool for the `main_backend' backend, and
%% executes the function in `Fun' passing `DBSession' as argument.
%%
%% Returns `{error, no_available_connection}' if a connection from the
%% pool cannot be resolved at the moment or it cannot connect to the
%% server database.
%%
%% Example:
%% <pre>
%%   connection_block(fun(DBSession)-> ... end).
%% </pre>
%% Note:<ul>
%% <li>
%%   It uses the backend's `wait_for_reusable_connection_timeout'
%%   option as a connection timeout.</li>
%% <li>
%%   The backend is configured in `cowboy_enhancer' environment
%%   variable called `database_manager'. i.e.:</li></ul>
%% <pre>
%%    {database_manager, [
%%        {main_backend, [
%%            {backend, postgres_backend},
%%            {server, "localhost"},
%%            {username, "postgres"},
%%            {password, "server"},
%%            {database, "eoc_db"},
%%            {max_reusable_connections, 10}, % 10 connections.
%%            {wait_for_reusable_connection_timeout, 5000} % 5 sec.
%%        ]}
%%    ]}
%% </pre>
%% Equivalent to:
%% <pre>
%%   connection_block(Fun, []).
%% </pre>
%% @see connection_block/2.
%% @end
%% -------------------------------------------------------------------
-spec connection_block(Fun) ->
    FunReply
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection} when
    Fun :: fun(),
    BackendName :: atom(),
    FunReply :: any().
connection_block(Fun) ->
    connection_block(Fun, []).

%% -------------------------------------------------------------------
%% @doc
%% Tries to create connection session with an available connection
%% from the connection pool for the specified `BackendName' in
%% `Options', and executes the function in `Fun' passing `DBSession'
%% as argument.
%%
%% Returns `{error, no_available_connection}' if a connection from the
%% pool cannot be resolved at the moment or it cannot connect to the
%% server database.
%%
%% It uses a `timeout' option in milliseconds to wait for an available
%% connection which overrides `wait_for_reusable_connection_timeout'
%% backend option.
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%   {timeout, Timeout :: timeout() | default_timeout}
%% '''
%% Example:
%% <pre>
%%   connection_block(fun(DBSession)-> ... end,
%%       [{backend, my_backend}, {timeout, default_timeout}]).
%% </pre>
%% Note:<ul>
%% <li>
%%   You can set `{timeout, default_timeout}' to use the backend's
%%   `wait_for_reusable_connection_timeout' option.</li>
%% <li>
%%   If the backend options is not specified `main_backend' config
%%   will be used.</li>
%% <li>
%%   The backend is configured in `cowboy_enhancer' environment
%%   variable called `database_manager'. i.e.:</li></ul>
%% <pre>
%%    {database_manager, [
%%        {main_backend, [
%%            {backend, postgres_backend},
%%            {server, "localhost"},
%%            {username, "postgres"},
%%            {password, "server"},
%%            {database, "eoc_db"},
%%            {max_reusable_connections, 10}, % 10 connections.
%%            {wait_for_reusable_connection_timeout, 5000} % 5 sec.
%%        ]}
%%    ]}
%% </pre>
%% @end
%% -------------------------------------------------------------------
-spec connection_block(Fun, Options) ->
    FunReply
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection} when
    Fun :: fun(),
    BackendName :: atom(),
    Options :: proplists:proplist(),
    FunReply :: any().
connection_block(Fun, Options) ->
    BackendName = proplists:get_value(backend, Options, main_backend),
    %% gets a working connection pool name for the specified 'BackendName'.
    case check_backend_connection_pool(BackendName) of
        {ok, ConnectionPool} ->
            Timeout = proplists:get_value(timeout, Options, default_timeout),
            get_connection_block(ConnectionPool, Fun, Timeout);
        Other ->
            Other
    end.

%% @private
get_connection_block(ConnectionPool, Fun, Timeout) ->
    %% tries to get an available connection from the 'Backend' connection pool.
    case case gen_server:call(ConnectionPool, {get_connection, Timeout}) of
        {ok, DBSession} ->
            {ok, DBSession};
        {error, no_available_connection, DefaultWaitTimeout} ->
            %% registers the caller to wait until timeout for an
            %% available connection.
            ok = gen_server:call(ConnectionPool,
                {register_caller, self()}),
            wait_for_available_connection(
                case Timeout of
                    default_timeout ->
                        DefaultWaitTimeout;
                    _ ->
                        Timeout
                end, ConnectionPool)
    end of
        {ok, DBSession2 = {_, Connection}} ->
            try
                %% executes the function with 'DBSession' as its arguments.
                Fun(DBSession2)
            after
                %% after function ended its execution no matter if errors occurs
                %% put connection back into the queue.
                case gen_server:call(ConnectionPool,
                    {free_connection, Connection})
                of
                    ok ->
                        ok;
                    {ask_connection, Pid} ->
                        %% alerts the next caller in the waiting queue to ask for
                        %% the just released connection.
                        erlang:send(Pid, ask_connection)
                end
            end;
        Other ->
            Other
    end.

%% @private
%% waits for an available connection or until a specified timeout.
wait_for_available_connection(TimeOut, ConnectionPool) ->
    receive
        ask_connection ->
            case gen_server:call(ConnectionPool, {get_connection, TimeOut}) of
                {ok, DBSession} ->
                    {ok, DBSession};
                {error, no_available_connection, _} ->
                    %% enters a loop until a connection is available or timeout.
                    ok = gen_server:call(ConnectionPool,
                        {register_caller, self()}),
                    wait_for_available_connection(TimeOut, ConnectionPool)
            end
    after TimeOut ->
        debug_logger:log_warning_msg(
            "Caller ~p got timeout waiting for an available connection", [self()]),
        %% unregisters the caller once it ends the waiting.
        ok = gen_server:call(ConnectionPool, {unregister_caller, self()}),
        {error, no_available_connection}
    end.

%% -------------------------------------------------------------------
%% @doc
%% Tries to create a connection session and a database transaction
%% context with an available connection from the connection pool for
%% the `main_backend' backend, and executes the function in `Fun'
%% passing `DBSession' as argument.
%%
%% Returns `{error, no_available_connection}' if a connection from the
%% pool cannot be resolved at the moment or it cannot connect to the
%% server database.
%%
%% Example:
%% <pre>
%%   connection_block_transaction(fun(DBSession)-> ... end).
%% </pre>
%% Note:<ul>
%% <li>
%%   It uses the backend `wait_for_reusable_connection_timeout'
%%   option as a connection timeout.</li>
%% <li>
%%   The backend is configured in `cowboy_enhancer' environment
%%   variable called `database_manager'. i.e.:</li></ul>
%% <pre>
%%    {database_manager, [
%%        {main_backend, [
%%            {backend, postgres_backend},
%%            {server, "localhost"},
%%            {username, "postgres"},
%%            {password, "server"},
%%            {database, "eoc_db"},
%%            {max_reusable_connections, 10}, % 10 connections.
%%            {wait_for_reusable_connection_timeout, 5000} % 5 sec.
%%        ]}
%%    ]}
%% </pre>
%% Equivalent to:
%% <pre>
%%   connection_block_transaction(Fun, []).
%% </pre>
%% @see connection_block_transaction/2.
%% @end
%% -------------------------------------------------------------------
-spec connection_block_transaction(Fun) ->
    FunReply
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, {rollback, Reason}} when
    Fun :: fun(),
    BackendName :: atom(),
    FunReply :: any(),
    Reason :: term().
connection_block_transaction(Fun) ->
    connection_block_transaction(Fun, []).

%% -------------------------------------------------------------------
%% @doc
%% Tries to create a connection session and a database transaction
%% context with an available connection from the connection pool for
%% the specified `BackendName' in `Options', and executes the function
%% in `Fun' passing `DBSession' as argument.
%%
%% Returns `{error, no_available_connection}' if a connection from the
%% pool cannot be resolved at the moment or it cannot connect to the
%% server database.
%%
%% It uses a `timeout' option in milliseconds to wait for an available
%% connection which overrides `wait_for_reusable_connection_timeout'
%% backend option.
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%   {timeout, Timeout :: timeout() | default_timeout}
%% '''
%% Example:
%% <pre>
%%   connection_block_transaction(fun(DBSession)-> ... end,
%%       [{backend, my_backend}, {timeout, default_timeout}]).
%% </pre>
%% Note:<ul>
%% <li>
%%   You can set `{timeout, default_timeout}' to use the backend's
%%   `wait_for_reusable_connection_timeout' option.</li>
%% <li>
%%   If the backend options is not specified `main_backend' config
%%   will be used.</li>
%% <li>
%%   The backend is configured in `cowboy_enhancer' environment
%%    variable called `database_manager'. i.e.:</li></ul>
%% <pre>
%%    {database_manager, [
%%        {main_backend, [
%%            {backend, postgres_backend},
%%            {server, "localhost"},
%%            {username, "postgres"},
%%            {password, "server"},
%%            {database, "eoc_db"},
%%            {max_reusable_connections, 10}, % 10 connections.
%%            {wait_for_reusable_connection_timeout, 5000} % 5 sec.
%%        ]}
%%    ]}
%% </pre>
%% @end
%% -------------------------------------------------------------------
-spec connection_block_transaction(Fun, Options) ->
    FunReply
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, {rollback, Reason}} when
    Fun :: fun(),
    BackendName :: atom(),
    Options :: proplists:proplist(),
    FunReply :: any(),
    Reason :: term().
connection_block_transaction(Fun, Options) ->
    connection_block(fun(DBSession) ->
        transaction(DBSession, fun() -> Fun(DBSession) end)
    end, Options).

%% -------------------------------------------------------------------
%% @doc
%% Makes a query to a database for the specified `DBSession'.
%%
%% Returns:
%% ```
%%   {ok, Data} %% when using 'select'.
%%   {ok, Count} % when using 'update', 'insert', 'delete'.
%%   {ok, Count, Data} %% when using 'update', 'insert', 'delete'
%%                        with returning.
%% '''
%% Example:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, N, [#{id := Id, name := Name} |_]} =
%%           database_manager:query(DBSession,
%%               "SELECT users.id, users.name FROM users
%%                   WHERE name=$1 and age=$2",
%%               ["John", 31])
%%   end).
%% </pre>
%% Note:<ul>
%% <li>
%%   Result data will be in map format.</li></ul>
%% @see query/4.
%% @end
%% -------------------------------------------------------------------
-spec query(DBSession, Query, Params) ->
    {ok, map_result_data()} |
    {ok, Count} |
    {ok, Count, map_result_data()}
    | {error, Reason} when
    DBSession :: db_session(),
    Query :: string() | bitstring(),
    Params :: query_params(),
    Count :: integer(),
    Reason :: term().
query({Backend, Connection}, Query, Params) ->
    %% executes 'query/4' in the current 'Backend'..
    Backend:query(Connection, Query, Params, []).

%% -------------------------------------------------------------------
%% @doc
%% Makes a query to a database for the specified `DBSession' using
%% `Options' to extend the functionality.
%%
%% Returns a `ResultData', it depends of `result_format' Option,
%% `result_format' could be one of `raw', `map', `proplist',
%% `map' and `proplist' result formats have the same
%% `ResultData' struct:
%% ```
%%   {ok, Data} %% when using 'select'.
%%   {ok, Count} % when using 'update', 'insert', 'delete'.
%%   {ok, Count, Data} %% when using 'update', 'insert', 'delete'
%%                        with returning.
%% '''
%% The only differences are:
%% ```
%%   0 result:
%%       'map' -> no_data,
%%       'proplist' -> []
%%   1 result:
%%       'map' -> a single map #{...},
%%       'proplist' -> a list of proplist [[...]], (templ. comp.)
%%   N result:
%%       'map' -> a list of maps [#{...}, ...],
%%       'proplist' -> a list of proplists  [[...], ...]
%% '''
%% `raw' by the other hand is `Backend' dependent.
%% ==== Options ====
%% ```
%%   {result_format, raw | map | proplist}.
%% '''
%% Example:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, N, [#{id := Id, name := Name} |_]} =
%%           database_manager:query(DBSession,
%%               "SELECT users.id, users.name FROM users
%%                   WHERE name=$1 and age=$2",
%%               ["John", 31], [{result_format, map}])
%%   end).
%% </pre>
%% Note:<ul>
%% <li>
%%   All result format has its pros and cons, `raw' is the fastest
%%   but backend dependent, `map' is the slowest but very easy to
%%   deal with. So this is the performance order from fastest to
%%   slowest `raw' > `proplist' > `map'.</li>
%% <li>
%%   `proplist' can be used to return directly to a template, also
%%   when you need to export the data to the world of other libs or
%%   through a socket `proplist' could be the best choice because it
%%   more standard than `map'.</li>
%% <li>
%%   If no `result_format' is specified, `map' will be the default
%%   result format.</li></ul>
%% @end
%% -------------------------------------------------------------------
-spec query(DBSession, Query, Params, Options) ->
    {ok, result_data()} |
    {ok, Count} |
    {ok, Count, result_data()}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    DBSession :: db_session(),
    Query :: string() | bitstring(),
    Params :: query_params(),
    Options :: proplists:proplist(),
    Other :: term().
query({Backend, Connection}, Query, Params, Options) ->
    %% executes 'query/4' in the current 'Backend'.
    Backend:query(Connection, Query, Params, Options).

%% -------------------------------------------------------------------
%% @doc
%% Prepares a transaction context for the specified `DBSession'.
%%
%% Example:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       database_manager:transaction(DBSession, fun() ->
%%           {ok, 1} =
%%               database_manager:query(DBSession,
%%                   InsertQuery, Params)
%%       end)
%%   end).
%% </pre>
%% @end
%% -------------------------------------------------------------------
-spec transaction(DBSession, Fun) ->
    FunReply
    | {error, {rollback, Reason}} when
    DBSession :: db_session(),
    Fun :: fun(),
    FunReply :: any(),
    Reason :: term().
transaction({Backend, Connection}, Fun) ->
    %% executes 'transaction/2' in the current 'Backend'.
    Backend:transaction(Connection, Fun).

%% -------------------------------------------------------------------
%% @doc
%% Inserts data using a `TableSpec' into a database.
%%
%% `TableSpec' has the following structure:<br/>
%% ```
%%   {table_name, FieldMap :: map()}
%% '''
%% `FieldMap' is a map with the fields names and values.
%%
%% Example:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, 1} =
%%           database_manager:insert(DBSession,
%%               {users, #{name => "john"}})
%%   end).
%% </pre>
%% Note:<ul>
%% <li>
%%   You set in a `TableSpec' a value of a field equal to another
%%   field name in order to get the same value of the specified
%%   field name. The order in the expression does not matter. i.e.:
%%   ```
%%   {users, #{
%%       name => "john",
%%       password => "server",
%%       old_password = password}}
%%   '''</li></ul>
%% Equivalent to:
%% <pre>
%%   insert(DBSession, TableSpec, []).
%% </pre>
%% @see insert/3.
%% @end
%% -------------------------------------------------------------------
-spec insert(DBSession, TableSpec) ->
    {ok, Count}
    | {error, Reason} when
    DBSession :: db_session(),
    TableSpec :: table_spec(),
    Count :: integer(),
    Reason :: term().
insert({Backend, Connection}, TableSpec) ->
    %% parses 'TableSpec' to find re-assignation of values.
    NewTableSpec = parse_table_spec(TableSpec),
    %% executes 'insert/3' in the current 'Backend'.
    Backend:insert(Connection, NewTableSpec, []).

%% -------------------------------------------------------------------
%% @doc
%% Inserts data using a `TableSpec' and `Options' into a database.
%%
%% `TableSpec' has the following structure:<br/>
%% ```
%%   {table_name, FieldMap :: map()}
%% '''
%% `FieldMap' is a map with the fields names and values.
%% ==== Options ====
%% ```
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {result_format,
%%       raw | map | proplist} %% For `return_fields'.
%%                                `map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Example 1: `{result_format, map}'.
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, 1, #{id := Id}} =
%%           database_manager:insert(DBSession,
%%               {users, #{name => "john"}},
%%                   [{return_fields, [id]}, {result_format, map}])
%%   end).
%% </pre>
%% Example 2: `{result_format, proplist}'.
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, 1, [[{id, Id}]] =
%%           database_manager:insert(DBSession,
%%               {users, #{name => "john"}},
%%                   [{return_fields, [id]},
%%                    {result_format, proplist}])
%%   end).
%% </pre>
%% Note:<ul>
%% <li>
%%   You set in a `TableSpec' a value of a field equal to another
%%   field name in order to get the same value of the specified
%%   field name. The order in the expression does not matter. i.e.:
%%   ```
%%   {users, #{
%%       name => "john",
%%       password => "server",
%%       old_password = password}}
%%   '''</li>
%% <li>
%%   The field data returned depends on `result_format' options.
%%   i.e.: for `map' is map containing the field names and values,
%%   and for `proplist' is a proplist with the field names and
%%   values inside a list for erlydtl compatibility aspects.</li>
%% <li>
%%   If no `result_format' is specified, `map' will be the default
%%   result format.</li></ul>
%% @end
%% -------------------------------------------------------------------
-spec insert(DBSession, TableSpec, Options) ->
    {ok, Count} |
    {ok, Count, result_data_for_insert()}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    DBSession :: db_session(),
    TableSpec :: table_spec(),
    Options :: proplists:proplist(),
    Other :: term().
insert({Backend, Connection}, TableSpec, Options) ->
    %% parses 'TableSpec' to find re-assignation of values.
    NewTableSpec = parse_table_spec(TableSpec),
    %% executes 'insert/3' in the current 'Backend'.
    Backend:insert(Connection, NewTableSpec, Options).

%% -------------------------------------------------------------------
%% @doc
%% Updates data in a database using a `TableSpec' and
%% `MatchFieldSpecs' parameters.
%%
%% `TableSpec' has the following structure:<br/>
%% ```
%%   {table_name, FieldMap :: map()}
%% '''
%% `FieldMap' is a map with the fields names and values. It must
%% contains only the fields you wish to update.
%%
%% `MatchFieldSpecs' is used to match against a sort of records for
%% update. Its structure is as follows:<br/>
%% ```
%%   [condition_term(), ...] or
%%   [condition_term(), logic_operator(), condition_term()]
%% '''
%% If the `logic_operator()' is omitted between two `condition_term()'
%% then the `and' logic operator will be used. i.e.: in the second
%% clause of the definition of a `MatchFieldSpecs' where a list of
%% condition_term() separated by comma is defined.
%%
%% You can also group `condition_term()' to apply a logic operator to
%% it. i.e.:
%% ```
%%   [[condition_term(), ...], logic_operator(), ...]
%% '''
%% You can use a lot of combinations while you bear in mind that some
%% logic operators require two `condition_term()' in each side and
%% others like `not' require only one.
%%
%% Example:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, N} =
%%           database_manager:update(DBSession,
%%               {users, #{signed => true}},
%%               [{name, 'eq', "john"}, {age, 'gt', 30}])
%%   end).
%% </pre>
%% Meaning: update all records with `name = "john"' to `signed = true'
%%          whose `age' are greater than `30'.
%%
%% Note:<ul>
%% <li>
%%   You set in a `TableSpec' a value of a field equal to another
%%   field name in order to get the same value of the specified
%%   field name. The order in the expression does not matter. i.e.:
%%   ```
%%   {users, #{
%%       name => "john",
%%       password => "server",
%%       old_password = password}}
%%   '''</li></ul>
%% @see update/4.
%% @end
%% -------------------------------------------------------------------
-spec update(DBSession, TableSpec, MatchFieldSpecs) ->
    {ok, Count}
    | {error, Reason} when
    DBSession :: db_session(),
    TableSpec :: table_spec(),
    MatchFieldSpecs :: match_field_specs(),
    Count :: integer(),
    Reason :: term().
update({Backend, Connection}, TableSpec, MatchFieldSpecs) ->
    % parses 'TableSpec' to find re-assignation of values.
    NewTableSpec = parse_table_spec(TableSpec),
    %% executes update/4 in the current 'Backend'.
    Backend:update(Connection, NewTableSpec, MatchFieldSpecs, []).

%% -------------------------------------------------------------------
%% @doc
%% Updates data in a database using a `TableSpec', `MatchFieldSpec'
%% and `Options', parameters.
%%
%% `TableSpec' has the following structure:<br/>
%% ```
%%   {table_name, FieldMap :: map()}
%% '''
%% `FieldMap' is a map with the fields names and values. It must
%% contains only the fields you wish to update.
%%
%% `MatchFieldSpecs' is used to match against a sort of records for
%% update. Its structure is as follows:<br/>
%% ```
%%   [condition_term(), ...] or
%%   [condition_term(), logic_operator(), condition_term()]
%% '''
%% If the `logic_operator()' is omitted between two `condition_term()'
%% then the `and' logic operator will be used. i.e.: in the second
%% clause of the definition of a `MatchFieldSpecs' where a list of
%% condition_term() separated by comma is defined.
%%
%% You can also group `condition_term()' to apply a logic operator to
%% it. i.e.:
%% ```
%%   [[condition_term(), ...], logic_operator(), ...]
%% '''
%% You can use a lot of combinations while you bear in mind that some
%% logic operators require two `condition_term()' in each side and
%% others like `not' require only one.
%%
%% Use `Options' to extend the functionality of the update.
%% ==== Options ====
%% ```
%%   {return_fields,
%%       [field_name() | full_field_name() |
%%        {TableName, [field_name(), ...]}, ...]}
%%
%%   {result_format,
%%       raw | map | proplist} %% For `return_fields'.
%%                                `map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Example:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, N, [#{id := Id} |_]} =
%%           database_manager:update(DBSession,
%%               {users, #{signed => true}},
%%               [{name, 'eq', "john"}, {age, 'gt', 30}],
%%               [{return_fields, [id]}, {result_format, map}])
%%   end).
%% </pre>
%% Meaning: update all records with `name = "john"' to `signed = true'
%%          whose `age' are greater than `30', returning its ids.
%%
%% Note:<ul>
%% <li>
%%   You set in a `TableSpec' a value of a field equal to another
%%   field name in order to get the same value of the specified
%%   field name. The order in the expression does not matter. i.e.:
%%   ```
%%   {users, #{
%%       name => "john",
%%       password => "server",
%%       old_password = password}}
%%   '''</li>
%% <li>
%%  To use `*' simple enclose it between `` ' ' ''. i.e.:
%%  ```
%%    {return_fields, ['*']} or
%%    {return_fields, [users, ['*']]} or
%%    {return_fields, [{users, '*'}]}
%%  '''</li></ul>
%% @end
%% -------------------------------------------------------------------
-spec update(DBSession, TableSpec, MatchFieldSpecs, Options) ->
    {ok, Count} |
    {ok, Count, result_data()}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    DBSession :: db_session(),
    TableSpec :: table_spec(),
    MatchFieldSpecs :: match_field_specs(),
    Options :: proplists:proplist(),
    Count :: integer(),
    Other :: term().
update({Backend, Connection}, TableSpec, MatchFieldSpecs, Options) ->
    % parses 'TableSpec' to find re-assignation of values.
    NewTableSpec = parse_table_spec(TableSpec),
    %% executes update/4 in the current 'Backend'.
    Backend:update(Connection, NewTableSpec, MatchFieldSpecs, Options).

%% -------------------------------------------------------------------
%% @doc
%% Deletes data from a database using a `TableName' and
%% `MatchFieldSpecs' parameters.
%%
%% `MatchFieldSpecs' is used to match against a sort of records to
%% delete. Its structure is as follows:<br/>
%% ```
%%   [condition_term(), ...] |
%%   [condition_term(), logic_operator(), condition_term()]
%% '''
%% If the `logic_operator()' is omitted between two `condition_term()'
%% i.e.: `[condition_term(), condition_term(), ...]' then the `and'
%% logic operator will be used.
%%
%% You can also group `condition_term()' to apply a logic operator to
%% it. i.e.:
%% ```
%%   [[condition_term(), ...], logic_operator(), ...]
%% '''
%% You can use a lot of combinations while you bear in mind that some
%% logic operators require two `condition_term()' in each side and
%% others like `not' require only one.
%%
%% Example:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, N} =
%%           database_manager:delete(DBSession, users,
%%               [{name, 'eq', "john"}, {age, 'gt', 30}])
%%   end).
%% </pre>
%% Meaning: delete all `users' records with `name = "john"' and `age'
%%          greater than `30'.
%%
%% @see delete/3.
%% @end
%% -------------------------------------------------------------------
-spec delete(DBSession, TableName, MatchFieldSpecs) ->
    {ok, Count}
    | {error, Reason} when
    DBSession :: db_session(),
    TableName :: atom(),
    MatchFieldSpecs :: match_field_specs(),
    Count :: integer(),
    Reason :: term().
delete({Backend, Connection}, TableName, MatchFieldSpecs) ->
    %% executes delete/4 in the current 'Backend'.
    Backend:delete(Connection, TableName, MatchFieldSpecs, []).

%% -------------------------------------------------------------------
%% @doc
%% Deletes data from a database using a `TableName', `MatchFieldSpecs'
%% and `Options' parameters.
%%
%% `MatchFieldSpecs' is used to match against a sort of records to
%% delete. Its structure is as follows:<br/>
%% ```
%%   [condition_term(), ...] |
%%   [condition_term(), logic_operator(), condition_term()]
%% '''
%% If the `logic_operator()' is omitted between two `condition_term()'
%% i.e.: `[condition_term(), condition_term(), ...]' then the `and'
%% logic operator will be used.
%%
%% You can also group `condition_term()' to apply a logic operator to
%% it. i.e.:
%% ```
%%   [[condition_term(), ...], logic_operator(), ...]
%% '''
%% You can use a lot of combinations while you bear in mind that some
%% logic operators require two `condition_term()' in each side and
%% others like `not' require only one.
%%
%% Use `Options' to extend the functionality of the delete function.
%% ==== Options ====
%% ```
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {result_format,
%%       raw | map | proplist} %% For `return_fields'.
%%                                `map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Example:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, N, [#{id := Id} |_]} =
%%           database_manager:delete(DBSession, users,
%%               [{name, 'eq', "john"}, {age, 'gt', 30}],
%%               [{return_fields, [id]}, {result_format, map}])
%%   end).
%% </pre>
%% Meaning: delete all `users' records with `name = "john"' and `age'
%%          greater than `30', returning its ids.
%%
%% Note:<ul>
%% <li>
%%  You will only get `result_data()' when using `return_fields'
%%  option.</li></ul>
%% @end
%% -------------------------------------------------------------------
-spec delete(DBSession, TableName, MatchFieldSpecs, Options) ->
    {ok, Count} |
    {ok, Count, result_data()}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    DBSession :: db_session(),
    TableName :: atom(),
    MatchFieldSpecs :: match_field_specs(),
    Options :: proplists:proplist(),
    Count :: integer(),
    Other :: term().
delete({Backend, Connection}, TableName, MatchFieldSpecs, Options) ->
    %% executes delete/4 in the current 'Backend'.
    Backend:delete(Connection, TableName, MatchFieldSpecs, Options).

%% -------------------------------------------------------------------
%% @doc
%% Finds a single occurrence of data in a database using `TableNames'
%% and `MatchFieldSpecs' parameters.
%%
%% Equivalent:
%% ```
%%   find(DBSession, TableNames, MatchFieldSpecs, [{limit, 1}]).
%% '''
%% @see find/4.
%% @end
%% -------------------------------------------------------------------
find_one({Backend, Connection}, TableNames, MatchFieldSpecs) ->
    find({Backend, Connection}, TableNames, MatchFieldSpecs, [{limit, 1}]).

%% -------------------------------------------------------------------
%% @doc
%% Finds a single occurrence of data in a database using `TableNames',
%% `MatchFieldSpecs' and `Options' parameters.
%%
%% Equivalent:
%% ```
%%   find(DBSession, TableNames, MatchFieldSpecs,
%%       [{limit, 1} | Options]).
%% '''
%% @see find/4.
%% @end
%% -------------------------------------------------------------------
find_one({Backend, Connection}, TableNames, MatchFieldSpecs, Options) ->
    find({Backend, Connection}, TableNames, MatchFieldSpecs, [{limit, 1} | Options]).

%% -------------------------------------------------------------------
%% @doc
%% Finds data in a database using `TableNames' and `MatchFieldSpecs'
%% parameters.
%%
%% Equivalent:
%% ```
%%   find(DBSession, TableNames, MatchFieldSpecs, []).
%% '''
%% @see find/4.
%% @end
%% -------------------------------------------------------------------

find({Backend, Connection}, TableNames, MatchFieldSpecs) ->
    find({Backend, Connection}, TableNames, MatchFieldSpecs, []).

%% -------------------------------------------------------------------
%% @doc
%% Finds data in a database using `TableNames', `MatchFieldSpecs' and
%% `Options' parameters.
%%
%% `MatchFieldSpecs' is used to match against a sort of records to
%% find. Its structure is as follows:<br/>
%% ```
%%   #{}              %% you an use a map for matching.
%%   [condition_term(), ...] |
%%   [condition_term(), logic_operator(), condition_term()]
%% '''
%% If the `logic_operator()' is omitted between two `condition_term()'
%% i.e.: `[condition_term(), condition_term(), ...]' then the `and'
%% logic operator will be used.
%%
%% You can also group `condition_term()' to apply a logic operator to
%% it. i.e.:
%% ```
%%   [[condition_term(), ...], logic_operator(), ...]
%% '''
%% You can use a lot of combinations while you bear in mind that some
%% logic operators require two `condition_term()' in each side and
%% others like `not' require only one.
%%
%% Use `Options' to extend the functionality of the find function.
%% ==== Options ====
%% ```
%%   {limit, Limit :: pos_integer()} %% Limits the number of records
%%                                      returned by find.
%%
%%   {offset, Offset :: pos_integer()} %% Returns records starting by
%%                                        'Offset'.
%%
%%   {order_by, order_by_expr()} %% Orders the result data by
%%                                  specific fields.
%%
%%   {group_by, group_by_expr()} %% Groups the result data by
%%                                  specific fields.
%%
%%   {return_fields,
%%       [field_name() | full_field_name() |
%%        {TableName, [field_name(), ...]}, ...]}
%%
%%   {result_format,
%%       raw | map | proplist} %% For `return_fields'.
%%                                `map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Example with maps:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, #{id := Id}} =
%%           database_manager:find(DBSession, users,
%%               #{name => "Steven", age => 10},
%%               [{return_fields, [id]}, {limit, 1}])
%%   end).
%% </pre>
%% Example 2:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, #{id := Id}} =
%%           database_manager:find(DBSession, users,
%%               [{name, 'eq', "john"}, {age, 'gt', 30}],
%%               [{return_fields, [id]}, {limit, 1}])
%%   end).
%% </pre>
%% Meaning: find data from `users' table which `name = "john"' and `age'
%%          greater than `30', returning its id and limiting the result
%%          to one record.
%%
%% Example 3:
%% <pre>
%%   database_manager:connection_block(fun(DBSession) ->
%%       {ok, #{user_id := UserId, role_id := RoleID}} =
%%           database_manager:find(DBSession, [users, roles]
%%               [{{users, level}, '==', {roles, role_level}}],
%%               [{return_fields,
%%                   [{users, id, user_id}, {roles, id, role_id}]}])
%%   end).
%% </pre>
%% Meaning: find data from `users' and `roles' tables where `level'
%%          column from `users' match with `role_level' column from
%%          `roles' table, it also does returns the id of both table
%%          records with an user defined alias for each one of them.
%%
%% Note:<ul>
%% <li>
%%  To use `*' simple enclose it between `` ' ' ''. i.e.:
%%  ```
%%    {return_fields, ['*']} or
%%    {return_fields, [users, ['*']]} or
%%    {return_fields, [{users, '*'}]}
%%  '''</li></ul>
%% @end
%% -------------------------------------------------------------------
-spec find(DBSession, TableNames, MatchFieldSpecs, Options) ->
    {ok, result_data()}
    | {error, {invalid_clause_option, term()}}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    DBSession :: db_session(),
    TableNames :: atom() | [atom(), ...],
    MatchFieldSpecs :: match_field_specs(),
    Options :: proplists:proplist(),
    Other :: term().
find({Backend, Connection}, TableNames, MatchFieldSpecs, Options) ->
    %% executes find/4 in the current 'Backend'.
    Backend:find(Connection, TableNames, MatchFieldSpecs, Options).

%%%-------------------------------------------------------------------
%%% Admin API Functions
%%%-------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc
%% Installs a new database backend and start a connection pool for it.
%%
%% Example:
%% <pre>
%%   ok = install_backend(my_backend, [
%%            {backend, mnesia_backend},
%%            {server, "localhost"},
%%            {username, "test"},
%%            {password, "server"},
%%            {database, "eoc_db"},
%%            {max_reusable_connections, 5},
%%            {wait_for_reusable_connection_timeout, 5000}]).
%% </pre>
%% Note:<ul>
%% <li>
%%   It is possible to omit `max_reusable_connections' and
%%   `wait_for_reusable_connection_timeout' options, default values
%%   will be provided.</li></ul>
%% @end
%% -------------------------------------------------------------------
-spec install_backend(BackendName, Options) ->
    ok
    | error
    | {error, Other} when
    BackendName :: atom(),
    Options :: proplists:proplist(),
    Other :: term().
install_backend(BackendName, Options) ->
    case connection_pool_starter(parse_backend_config([{BackendName, Options}])) of
        {ok, 1} ->
            lager:info("Backend: ~p added!", [BackendName]),
            ok;
        _ ->
            lager:error("No backend added!"),
            error
    end.

%% -------------------------------------------------------------------
%% @doc
%% Removes a database backend specified by `BackendName' parameter.
%%
%% Example:
%% <pre>
%%   ok = remove_backend(my_backend).
%% </pre>
%% @end
%% -------------------------------------------------------------------
-spec remove_backend(BackendName) ->
    ok
    | {error, {invalid_backend, BackendName}}
    | {error, Other} when
    BackendName :: atom(),
    Other :: term().
remove_backend(BackendName) ->
    ConnectionPool = make_connection_pool_name(BackendName),
    case erlang:whereis(ConnectionPool) of
        undefined ->
            {error, {invalid_backend, BackendName}};
        Pid ->
            supervisor:terminate_child(?CONNECTION_POOL_SUP_NAME, Pid)
    end.

%% -------------------------------------------------------------------
%% @doc
%% Updates a backend configuration.
%%
%% Example:
%% <pre>
%%   ok = update_backend_options(my_backend, [
%%            {backend, mnesia_backend},
%%            {server, "localhost"},
%%            {username, "test"},
%%            {password, "server"},
%%            {database, "eoc_db"},
%%            {max_reusable_connections, 5},
%%            {wait_for_reusable_connection_timeout, 5000}]).
%% </pre>
%% @end
%% -------------------------------------------------------------------
-spec update_backend_options(BackendName, NewOptions) ->
    ok
    | {error, {invalid_backend, BackendName}}
    | {error, {invalid_options, [atom(), ...]}}
    | {error, Other} when
    BackendName :: atom(),
    NewOptions :: proplists:proplist(),
    Other :: term().
update_backend_options(BackendName, NewOptions) ->
    case check_backend_connection_pool(BackendName) of
        {ok, ConnectionPool} ->
            gen_server:call(ConnectionPool, {update_options, NewOptions});
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, infinity}]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, _} = supervisor:start_link({local, ?CONNECTION_POOL_SUP_NAME}, connection_pool_sup, []),
    %% gets all the config for database manager.
    %% TODO: check this if cowboy_enhancer changes its name in future for any reason.
    case config_manager:database_manager_config() of
        {ok, BackendsConfig} ->
            case parse_backend_config(BackendsConfig) of
                [] ->
                    debug_logger:log_error(
                        erlang:get_stacktrace(),
                        "There is no database backend to use!"),
                    {stop, {error, no_available_database_backend}};
                NewBackendsConfig ->
                    %% starts a connection pool for each configured backend.
                    {ok, _} = connection_pool_starter(NewBackendsConfig),
                    {ok, #state{}}
            end;
        undefined ->
            debug_logger:log_error(
                erlang:get_stacktrace(),
                "No database backend configuration was specified!"),
            {stop, {error, no_available_database_backend}}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

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

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Gets a list well configured backends.
%% @end
%% -------------------------------------------------------------------
-spec parse_backend_config(ListOfBackends) ->
    [BackendMap, ...] | [] when
    ListOfBackends :: proplists:proplist(),
    BackendMap :: map().
parse_backend_config(ListOfBackends) ->
    lists:foldl(fun({BackendName, Options}, Acc) ->
        Backend = proplists:get_value(backend, Options),
        Server = proplists:get_value(server, Options),
        Username = proplists:get_value(username, Options),
        Password = proplists:get_value(password, Options),
        Database = proplists:get_value(database, Options),
        %% gets max connection pool size.
        MaxConnections =
            case proplists:get_value(max_reusable_connections, Options) of
                undefined -> ?MAX_REUSABLE_CONNECTIONS;
                Value -> Value
            end,
        %% gets the max time in milliseconds to wait for an available connection.
        WFRCTimeOut =
            case proplists:get_value(wait_for_reusable_connection_timeout, Options) of
                undefined -> ?WAIT_FOR_REUSABLE_CONNECTION_TIMEOUT;
                Value2 -> Value2
            end,
        case (Backend =:= undefined) or (Server =:= undefined) or (Username =:= undefined)
            or (Password =:= undefined) or (Database =:= undefined)
        of
            true ->
                debug_logger:log_warning_msg(
                    "database backend defined under '~p' name has an incomplete "
                    "configuration. Not using it!", [BackendName]),
                Acc;
            false ->
                %% returns a list with all well configured backends.
                [{BackendName, [
                    {backend, Backend},
                    {server, Server},
                    {username, Username},
                    {password, Password},
                    {database, Database},
                    {max_reusable_connections, MaxConnections},
                    {wait_for_reusable_connection_timeout, WFRCTimeOut}]} | Acc]
        end
    end, [], ListOfBackends).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts a connection pool for each Configured Backend.
%% Returns: {ok, Count} which is the amount of pools started.
%% @end
%%--------------------------------------------------------------------
-spec connection_pool_starter(BackendsConfig) ->
    {ok, Count} when
    BackendsConfig :: [{BackendName, [{OptionName, OptionValue}, ...]}],
    BackendName :: atom(),
    OptionName :: atom(),
    OptionValue :: any(),
    Count :: pos_integer().
connection_pool_starter(BackendsConfig) ->
    C =
        lists:foldl(fun({BackendName, Options}, Count) ->
            ConnectionPool = make_connection_pool_name(BackendName),
            %% starts a new supervised backend connection pool.
            case supervisor:start_child(?CONNECTION_POOL_SUP_NAME,
                [ConnectionPool, Options])
            of
                {ok, _} ->
                    Count;
                {error, {already_started, _Child}} ->
                    debug_logger:log_warning_msg(
                        "Duplicated backend configuration found!~n"
                        "           You have configured multiple '~p' backends.~n"
                        "           Only the first one will be used", [BackendName]),
                    Count - 1
            end
        end, length(BackendsConfig), BackendsConfig),
    {ok, C}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Gets a working connection pool name for the specified 'Backend'.
%% @end
%% -------------------------------------------------------------------
-spec check_backend_connection_pool(BackendName) ->
    {ok, ConnectionPool}
    | {error, invalid_backend} when
    BackendName :: atom(),
    ConnectionPool :: atom().
check_backend_connection_pool(BackendName) ->
    ConnectionPool = make_connection_pool_name(BackendName),
    case erlang:whereis(ConnectionPool) of
        undefined ->
            {error, {invalid_backend, BackendName}};
        _Pid ->
            {ok, ConnectionPool}
    end.

make_connection_pool_name(BackendName) ->
    list_to_atom("ce_" ++ atom_to_list(BackendName) ++ "_connection_pool").

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Gets a `NewTableSpec' with valued re-assigned. i.e. for:
%%   {users, #{
%%       name = "John",
%%       password = "server",
%%       old_password = password}}.
%% it will return:
%%   {users, #{
%%       name = "John",
%%       password = "server",
%%       old_password = "server"}}.
%% @end
%% -------------------------------------------------------------------
-spec parse_table_spec(TableSpec) ->
    NewTableSpec when
    TableSpec :: table_spec(),
    NewTableSpec :: table_spec().
parse_table_spec({Tab, DataMap}) ->
    Keys = maps:keys(DataMap),
    ParsedDataMap =
        maps:map(fun(_K, V) ->
            case lists:member(V, Keys) of
                true ->
                    maps:get(V, DataMap);
                false ->
                    V
            end
        end, DataMap),
    {Tab, ParsedDataMap}.





%% TODO: backend config store. so when process restart it can retrieve the installed backends.


init_mnesia_for_config_store() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(backend_config, [{disc_copies, [node()]}, {type, set},
        {attributes, record_info(fields, backend_config)}]),
    mnesia:wait_for_tables([backend_config], 5000).

store_config({Backend, Options}) ->
    #{
        server := Server,
        username := Username,
        password := Password,
        database := Database,
        max_reusable_connections := MaxConnections,
        wait_for_reusable_connection_timeout := WaitForConnectionTimeout} = Options,
    mnesia:transaction(fun() ->
        mnesia:write(#backend_config{
            backend = Backend,
            server = Server,
            username = Username,
            password = Password,
            database = Database,
            max_reusable_connections = MaxConnections,
            wait_for_reusable_connection_timeout = WaitForConnectionTimeout})
    end).

get_config(Backend) ->
    case mnesia:transaction(fun() ->
        mnesia:read({backend_config, Backend})
    end) of
        {atomic, []} ->
            {error, {invalid_backend, Backend}};
        {atomic, Config} ->
            io:format("*** Backend config: ~p~n", [Config]);
        {aborted, Reason} ->
            {error, Reason}
    end.