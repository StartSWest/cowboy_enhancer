-module(cowboy_enhancer_app).
-behavior(application).

%% API Exports.
-export([start_dev/0, start/0]).

%% Application Callbacks.
-export([start/2, stop/1]).

%% -------------------------------------------------------------------
%% API Functions.
%% -------------------------------------------------------------------

start() ->
    start_verbose(100).

start_dev() ->
    start_verbose(0),
    observer:start().

start_verbose(VerboseTime) ->
    %% prepares code paths.
    %%prepare_code(VerboseTime),

    verbose_log(1, "~nStarting 'cowboy_enhancer' framework...~n"),
    %% starts the framework.
    %%{ok, _} = application:ensure_all_started(cowboy_enhancer),
    %% ensuring that other code remains sticky.

    [code:stick_mod(M) || {M, F} <- code:all_loaded(),
        is_list(F), not code:is_sticky(M)],

    %% This is a patch to suppress "meta info is out of date" warning
    %% for observer module and deps.
    %% TODO: deal with this better.
    code:stick_dir(filename:dirname(code:which(observer))),
    code:stick_dir(filename:dirname(code:which(wx))),
    timer:sleep(500),

    %% compiles the templates.
    compile_templates(VerboseTime),

    %% pre loads all controller modules.
    load_controllers(VerboseTime),
    verbose_log(1, "~nStarting system...~n"),
    timer:sleep(20 * VerboseTime),
    case config_manager:target_app() of
        {ok, App} ->
            %% starts the target application.
            %%{ok, _} = application:ensure_all_started(App),
            timer:sleep(500),
            verbose_log(0, "~nSystem ready.~n");
        undefined ->
            debug_logger:log_error_msg("No target application configured! See 'app.config'.~n"),
            timer:sleep(1000),
            application:stop(cowboy_enhancer),
            erlang:exit(exit)
    end.

%% -------------------------------------------------------------------
%% Application Callbacks
%% -------------------------------------------------------------------

-ifdef(dev_mode).
check_dev() ->
    io:format("~n~n *** DEVELOPMENT MODE ENABLED! ***~n~n").
-else.
check_dev() ->
    ok.
-endif.

start(_Type, _Args) ->
    check_dev(),
    start_verbose(10),

    debug_logger:log_info_msg("*** Compilling the Sources."),
    ce_control:rebuild_all(),

    observer:start(),

    %% Starts the supervisor.
    cowboy_enhancer_sup:start_link().

stop(_State) ->
    verbose_log(1, "~n~nStopping system..."),
    %%unprepare_code(),
    ok.

%% -------------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------------

%% @private
%% @doc
%% Prepares the code paths for the application and deps.
%% @end
prepare_code(VerboseTime) ->
    verbose_log(2, "~n~nGetting Application directory...done.~n"),
    {ok, AppDir} = file:get_cwd(),

    verbose_log(2, "~nAdding application directory to Erlang code path..."),
    timer:sleep(5 * VerboseTime),

    code:add_path(AppDir ++ "/ebin"),
    verbose_log(2, "done.~n"),

    verbose_log(2, "~nEntering to deps directory...~n"),
    DepsDir = filename:join(AppDir, "deps"),
    verbose_log(2, "  => ~p~n", [DepsDir]),

    verbose_log(2, "~nGetting deps' /ebin directory..."),
    timer:sleep(5 * VerboseTime),
    {ok, DepsDirs} = file:list_dir_all(DepsDir),
    EBinDirs = lists:map(fun(D) -> DepsDir ++ "/" ++ D ++ "/ebin" end, DepsDirs),
    verbose_log(2, "done.~n"),

    verbose_log(2, "~nAdding /deps to Erlang code path...~n"),
    timer:sleep(5 * VerboseTime),
    lists:foreach(fun(D) ->
        timer:sleep(VerboseTime),
        DD = string:tokens(D, "/"),
        [EBin, Deps | _] = lists:reverse(DD),
        case code:add_path(D) of
            {error, bad_directory} ->
                pass;
            true ->
                verbose_log(2, "  => ~s...done.~n", [Deps ++ "/" ++ EBin]);
            Error ->
                verbose_log(2, "  => ~s...~p~n", [Deps ++ "/" ++ EBin, Error])
        end
    end, EBinDirs),

    %% stores the code path information to be removed later when application stops.
    application:set_env(?MODULE, code_paths, EBinDirs).

unprepare_code() ->
    verbose_log(1, "~n~nRemoving code paths...~n"),
    timer:sleep(500),
    {ok, DirsBin} = application:get_env(?MODULE, code_paths),
    lists:foreach(fun(D) ->
        timer:sleep(100),
        DD = string:tokens(D, "/"),
        [EBin, Deps | _] = lists:reverse(DD),
        case code:del_path(D) of
            {error, bad_directory} ->
                pass;
            true ->
                verbose_log(3, "  => ~s...done.~n", [Deps ++ "/" ++ EBin]);
            Error ->
                verbose_log(2, "  => ~s...~p~n", [Deps ++ "/" ++ EBin, Error])
        end
    end, DirsBin),
    verbose_log(1, "done.~n").

compile_templates(VerboseTime) ->
    verbose_log(1, "~nCompiling templates...~n"),
    %% output directory for compiled templates.
    {ok, OutDir, Source} = config_manager:get_target_app_ebin_and_src(),
    %% source code directory for templates.
    {ok, RelTemplateDir} = config_manager:get_templates_dir(),
    TemplatesDir = filename:join([Source ++ "/" ++ RelTemplateDir]),
    filelib:fold_files(TemplatesDir, ".html", true, fun(File, _Acc) ->
        timer:sleep(VerboseTime),
        TN = filename:basename(filename:rootname(File)) ++ "_dtl",
        verbose_log(3, "  => ~p => ~p...", [filename:basename(File), TN]),
        CR = erlydtl:compile_file(File, TN, [force_recompile, debug_info, return_errors,
            {out_dir, OutDir}]),
        case CR of
            {ok, _} ->
                verbose_log(3, "done.~n");
            error ->
                verbose_log(2, "error!.~n"),
                exit(exit);
            {error, Errors, _Warnings} ->
                verbose_log(2, "error: ~p~n.", [Errors]),
                exit(exit)
        end,
        []
    end, []),
    timer:sleep(2 * VerboseTime).

load_controllers(VerboseTime) ->
    verbose_log(1, "~nLoading modules...~n"),
    {ok, OutDir, Source} = config_manager:get_target_app_ebin_and_src(),
    filelib:fold_files(OutDir, ".beam", true, fun(File, _Acc) ->
        timer:sleep(VerboseTime),
        FN = list_to_atom(filename:basename(filename:rootname(File))),
        verbose_log(3, "  => ~p...", [FN]),
        case code:ensure_loaded(FN) of
            {module, _} ->
                verbose_log(3, "done.~n");
            {error, _} ->
                verbose_log(2, "error!.~n")
        end,
        []
    end, []),
    timer:sleep(2 * VerboseTime).

verbose_log(VerboseLevelTarget, Format) ->
    verbose_log(VerboseLevelTarget, Format, []).
verbose_log(VerboseLevelTarget, Format, Args) ->
    VerboseLevel = config_manager:system_start_verbose_level(),
    case ((VerboseLevel >= VerboseLevelTarget) or (VerboseLevelTarget =< 0)) of
        true ->
            io:format(Format, Args);
        _ ->
            ok
    end.