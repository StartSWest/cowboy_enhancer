%%-------------------------------------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2017, Ivanco Software Corporation
%%
%% @doc
%% Config manager to retrieve application set of configurations.
%% @end
%% Created : 20. dic 2015 01:15 PM
%%-------------------------------------------------------------------------------------------------
-module(ce_config).
-author("Ivan Carmenates Garcia").

%%=================================================================================================
%% API Exports
%%=================================================================================================
-export([
  get_database_manager_config/0,
  get_session_manager_config/0,
  set_session_manager_config/1,
  get_target_app/0,
  get_templates_dir/0,
  get_target_app_ebin_and_src_dir/0,
  get_system_start_verbose_level/0]).

%%=================================================================================================
%% MACRO Definition
%%=================================================================================================
-define(ROOT_CONFIG_NAME, cowboy_enhancer).

%%=================================================================================================
%% API Functions
%%=================================================================================================

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves the related config for `database_manager' module.
%%
%% This section is configured by setting the code bellow in `app.config' file under 
%% `cowboy_enhancer' tag.
%%
%% <pre>
%%   {cowboy_enhancer, [
%%     ...
%%     % Database backends configuration.
%%     {database_manager, [
%%       {main_backend, [
%%         {backend, postgres_backend},
%%         {server, "localhost"},
%%         {username, "postgres"},
%%         {password, "server"},
%%         {database, "my_app_db"},
%%
%%         % max amount of database connections in the connection pool.
%%         {max_reusable_connections, 10}, % 10 connections.
%%
%%         % max time to wait for an available connection.
%%         {wait_for_reusable_connection_timeout, 10000} % 10 seconds.
%%       ]}
%%     ]},
%%     ...
%%   ]}
%% </pre>
%%
%% Note:<ul>
%% <li>
%%  You can configure multiple database backends. Like `main_backend' in the example above. You can 
%%  create other tags for other database backends and use them in your code.</li></ul>
%%
%% See `database_manager' module for more information about how to use backends.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_database_manager_config()
  -> {ok, Config}
   | undefined
  when
    Config :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
get_database_manager_config() ->
  application:get_env(?ROOT_CONFIG_NAME, database_manager).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves the related config for `session_manager' module.
%%
%% This section is configured by setting the code bellow in `app.config' file under 
%% `cowboy_enhancer' tag.
%%
%% <pre>
%%   {cowboy_enhancer, [
%%     ...
%%     % Configuration for session manager module.
%%     {session_manager, [
%%       % this is the default time in which sessions will expire.
%%       {session_expire_time, 360000}, % 1 hour.
%%
%%       % this is the default frequency in which expired sessions will be recollected and deleted.
%%       {garbage_collector_frequency, 360000}, % 1 hour.
%%     ]}
%%     ...
%%   ]}
%%
%% </pre>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_session_manager_config()
  -> {ok, Config}
   | undefined
  when
    Config :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
get_session_manager_config() ->
  application:get_env(?ROOT_CONFIG_NAME, session_manager).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Sets session manager configuration.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec set_session_manager_config(Config)
  -> ok
  when
    Config :: tuple() | list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
set_session_manager_config(Config) when is_tuple(Config) ->
  set_session_manager_config([Config]);

set_session_manager_config(Config) when is_list(Config) ->
  % Validates session manager configuration.
  session_manager:validate_config(Config),

  % gets old config.
  {ok, OldConfig} = get_session_manager_config(),
  OldConfigMap = maps:from_list(OldConfig),

  % merges old and new config in a way that new config is kept.
  ConfigMap = maps:from_list(Config),
  NewConfig = maps:to_list(maps:merge(OldConfigMap, ConfigMap)),

  % updates config on session_manager gen server.
  SERVER = session_manager:get_server_name(),
  ok = gen_server:call(SERVER, {update_config, NewConfig}, infinity),

  % updates config in the environment.
  ok = application:set_env(?ROOT_CONFIG_NAME, session_manager, NewConfig).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves the name of the target application configured for the Cowboy Enhancer framework.
%%
%% This section is configured by setting the code bellow in `app.config' file under 
%% `cowboy_enhancer' tag.
%%
%% <pre>
%%   {cowboy_enhancer, [
%%     ...
%%     % This is the main application name.
%%     {target_app, my_app},
%%     ...
%%   ]}
%% </pre>
%%
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_target_app()
  -> {ok, TargetApp}
   | undefined
  when
    TargetApp :: atom().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
get_target_app() ->
  application:get_env(?ROOT_CONFIG_NAME, target_app).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets erlydtl templates directory usually under '/view/templates'.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_templates_dir()
  -> {ok, TemplatesDir}
   | undefined
  when
    TemplatesDir :: string().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
get_templates_dir() ->	
  application:get_env(?ROOT_CONFIG_NAME, templates_dir).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets target app ebin and src directory.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_target_app_ebin_and_src_dir()
  -> {ok, EbinDir, SourceDir}
   | {error, could_no_resolve_target_app}
  when
    EbinDir :: string(),
    SourceDir :: string().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
get_target_app_ebin_and_src_dir() ->
  {ok, TargetApp} = get_target_app(),

  % tries with the current app name.
  R = case (catch (TargetApp:module_info(compile))) of
    {'EXIT', _} ->
      % If fail let's try again by adding '_app'.
      App = list_to_atom(atom_to_list(TargetApp) ++ "_app"),
      
      case (catch (App:module_info(compile))) of
        {'EXIT', _} ->
          {error, could_no_resolve_target_app};
        % found target app by adding '_app'.
        CompileInfo2 ->
          {ok, CompileInfo2}
      end;
      % found target app at first try.
    CompileInfo3 ->
      {ok, CompileInfo3}
  end,

  % parses app module info.
  case R of
    {ok, CompileInfo} ->
      % gets source directory.
      SourceDir = filename:dirname(proplists:get_value(source, CompileInfo, "")),

      % gets ebin directory.
      Options = proplists:get_value(options, CompileInfo, []),
      EbinDir = proplists:get_value(outdir, Options, []),

      {ok, EbinDir, SourceDir};
    Other ->
      Other
  end.
  %%-------------------------------------------------------------------------------------------------
%% @doc
%% Retrieves the initial level of detail of the debug information for the current system.
%%
%% This section is configured by setting the code bellow in `app.config' file under 
%% `cowboy_enhancer' tag.
%%
%% <pre>
%%   {cowboy_enhancer, [
%%     ...
%%     % shows more info in the console when system starts, more bigger the number is more detailed 
%%     % info is shown.
%%     % 0 -> no info, 3 -> maximum details.
%%     {system_start_verbose_level, 2},
%%     ...
%%   ]}
%% </pre>
%%
%% @end
%%-------------------------------------------------------------------------------------------------
-spec get_system_start_verbose_level()
  -> Level
   | undefined
  when
    Level :: number().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
get_system_start_verbose_level() ->
  application:get_env(?ROOT_CONFIG_NAME, system_start_verbose_level, 2).