-module(cowboy_enhancer_sup).
-behavior(supervisor).

%% Application Callbacks.
-export([start_link/0]).
-export([init/1]).

%% -------------------------------------------------------------------
%% Application Callbacks.
%% -------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Creates a session ets table to store the sessions.
    %% NOTE: It have to be created here and not in the session_manager gen_server
    %% to avoid the table to be destroyed if the gen_server fails and
    %% have to be restarted.
    EtsTableName = session_manager:get_ets_table_name(),
    ets:new(EtsTableName, [named_table, set, public, {keypos, 2},
        {read_concurrency, true}, {write_concurrency, true}]),

    {ok, {{one_for_one, 10, 10}, [
        {ce_security_lib,
            {security_lib, start_link, []},
            permanent,
            10000,
            worker,
            [security_lib]
        },
        {ce_session_manager,
            {session_manager, start_link, []},
            permanent,
            10000,
            worker,
            [session_manager]
        },
        {ce_database_manager,
            {database_manager, start_link, []},
            permanent,
            10000,
            worker,
            [database_manager]
        }
    ]}}.