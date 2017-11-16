%%-------------------------------------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2016, Ivanco Software Corporation
%% @doc
%% Config manager to retrieve application set of configurations.
%% @end
%% Created : 20. dic 2015 01:15 PM
%%-------------------------------------------------------------------------------------------------
-module(config_manager).
-author("Ivan Carmenates Garcia").

-define(ROOT_CONFIG_NAME, cowboy_enhancer).

%% API Export
-export([
  database_manager_config/0,
  session_manager_config/0,
  target_app/0,
	get_templates_dir/0,
	get_target_app_ebin_and_src/0,
	system_start_verbose_level/0,
  change_session_manager_config/1]).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves the related config for `database_manager' module.
%%
%% This section is configured by setting the code bellow in
%% `app.config' file under `cowboy_enhancer' tag:
%% <pre>
%%   {cowboy_enhancer, [
%%       ...
%%       %% Database backends configuration.
%%       {database_manager, [
%%           {main_backend, [
%%               {backend, postgres_backend},
%%               {server, "localhost"},
%%               {username, "postgres"},
%%               {password, "server"},
%%               {database, "my_app_db"},
%%               %% max amount of database connections in the
%%               %% connection pool.
%%               {max_reusable_connections, 10}, % 10 connections.
%%               %% max time to wait for an available connection.
%%               {wait_for_reusable_connection_timeout, 10000} % 10 seconds.
%%           ]}
%%       ]},
%%       ...
%%   ]}
%% </pre>
%% Note:<ul>
%% <li>
%%  You can configure multiple database backends. Like `main_backend'
%%  in the example above you can create other tags for other database
%%  backends and use them in an indistinct manner.</li></ul>
%%
%% See `database_manager' module for more information about how to use
%% backends.
%% @end
%%-------------------------------------------------------------------------------------------------
database_manager_config() ->
    application:get_env(?ROOT_CONFIG_NAME, database_manager).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves the related config for `session_manager' module.
%%
%% This section is configured by setting the code bellow in
%% `app.config' file under `cowboy_enhancer' tag:
%% <pre>
%%   {cowboy_enhancer, [
%%       ...
%%       %% Configuration for session manager module.
%%       {session_manager, [
%%           %% this is the default time in which sessions will expire.
%%           {session_expire_time, 360000}, % 1 hour.
%%
%%           %% this is the default frequency in which expired sessions will be
%%           %% recollected and deleted.
%%           {garbage_collector_frequency, 360000}, % 1 hour.
%%       ]}
%%       ...
%%   ]}
%% </pre>
%% @end
%%-------------------------------------------------------------------------------------------------
session_manager_config() ->
    application:get_env(?ROOT_CONFIG_NAME, session_manager).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves the initial level of detail of the debug information for
%% the current system.
%%
%% This section is configured by setting the code bellow in
%% `app.config' file under `cowboy_enhancer' tag:
%% <pre>
%%   {cowboy_enhancer, [
%%       ...
%%       %% shows more info in the console when system starts,
%%       %% more bigger the number is more detailed info is shown.
%%       %% 0 -> no info, 3 -> maximum details.
%%       {system_start_verbose_level, 2},
%%       ...
%%   ]}
%% </pre>
%% @end
%%-------------------------------------------------------------------------------------------------
system_start_verbose_level() ->
    application:get_env(?ROOT_CONFIG_NAME, system_start_verbose_level, 2).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves the name of the target application configured for the
%% Cowboy Enhancer framework.
%%
%% This section is configured by setting the code bellow in
%% `app.config' file under `cowboy_enhancer' tag:
%% <pre>
%%   {cowboy_enhancer, [
%%       ...
%%       %% This is the main application name.
%%       {target_app, my_app},
%%       ...
%%   ]}
%% </pre>
%% @end
%%-------------------------------------------------------------------------------------------------
target_app() ->
    application:get_env(?ROOT_CONFIG_NAME, target_app).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets templates directory.
%% @end
%%-------------------------------------------------------------------------------------------------
get_templates_dir() ->	
	application:get_env(?ROOT_CONFIG_NAME, templates_dir).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets target app directory.
%% @end
%%-------------------------------------------------------------------------------------------------
get_target_app_ebin_and_src() ->
  {ok, TargetApp} = target_app(),

  R = case (catch (TargetApp:module_info())) of
		{'EXIT', _} ->
			App = list_to_atom(atom_to_list(TargetApp) ++ "_app"),
			
			case (catch (App:module_info())) of
				{'EXIT', _} ->
					{error, could_no_resolve_app_path};

				ModuleInfo2 ->
					{ok, ModuleInfo2}
			end;

		ModuleInfo3 when is_list(ModuleInfo3) ->
			{ok, ModuleInfo3}
	end,

	case R of
		{ok, ModuleInfo} ->
			CompileInfo = proplists:get_value(compile, ModuleInfo, []),
			Source = filename:dirname(
				proplists:get_value(source, CompileInfo, "")),

			Options = proplists:get_value(options, CompileInfo, []),
			OutDir = proplists:get_value(outdir, Options, []),
			{ok, OutDir, Source};

		Other ->
			Other
	end.

change_session_manager_config(Config) when is_tuple(Config) ->
    change_session_manager_config([Config]);

change_session_manager_config(Config) when is_list(Config) ->
    CheckedConfig =
        lists:map(
            fun({Key, Value})
			when is_integer(Value),
			     Value > 0,
			     Key == session_expire_time;
				 Key == garbage_collector_frequency
			->
                {Key, Value}
            end, Config),

    {ok, OldConfig} = session_manager_config(),
    OldConfigMap = maps:from_list(OldConfig),

    CheckedConfigMap = maps:from_list(CheckedConfig),
    NewConfig = maps:to_list(maps:merge(OldConfigMap, CheckedConfigMap)),

    SERVER = session_manager:get_server_name(),
    ok = gen_server:call(SERVER, {change_config, NewConfig}, infinity),

    application:set_env(?ROOT_CONFIG_NAME, session_manager, NewConfig).