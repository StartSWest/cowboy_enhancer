%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% Control module for hot code rebuild and load.
%%% @end
%%% Created : 24. Aug 2015 10:59 AM
%%%-------------------------------------------------------------------
-module(ce_control).
-author("Ivan Carmenates Garcia").

%% -------------------------------------------------------------------
%% API Exports
%% -------------------------------------------------------------------
-export([rebuild_all/0, rebuild/1]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Rebuilds and loads all recently modified files. This is the way
%% to update the system once it is running.
%% @end
%%--------------------------------------------------------------------
-spec rebuild_all() -> ok | error.
rebuild_all() ->
    %% tries to rebuild all non-sticky recently modified project files.
    L = [rebuild(M) || {M, F} <- code:all_loaded(),
        is_list(F), not code:is_sticky(M)],
    %% if all files compiled ok, return ok, otherwise error.
    %% NOTE: {error, out_of_date_beam} isn't considered an error.
    case lists:all(fun(X) -> (ok == X) or (X == {error, out_of_date_beam}) end, L) of
        true ->
            ok;
        false ->
            error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Rebuilds and loads a recently modified module or erlydtl template.
%% @end
%%--------------------------------------------------------------------
-spec rebuild(Module :: atom()) ->
    ok | {error, out_of_date_beam} | {error, Errors :: list()}.
rebuild(Module) ->
    %% gets the module beam path.
    UModBeamPath = string:to_upper(code:which(Module)),
    %% ensures that a module is a compiled one.
    case UModBeamPath of
        [] ->
            ok;
        _ ->
            rebuild2(Module, UModBeamPath)
    end.

rebuild2(Module, UModBeamPath) ->
    %% retrieves the module metadata.
    ModInfo = Module:module_info(),
    %% gets the compile info metadata.
    CompileInfo = proplists:get_value(compile, ModInfo),
    %% gets compiler options.
    CompileOpts = proplists:get_value(options, CompileInfo),
    %% gets the compiled time.
    PreCompileTime = proplists:get_value(time, CompileInfo),
    {Y, Mo, D, H, Mi, S} = PreCompileTime,
    CompileTime = {{Y, Mo, D}, {H, Mi, S}},
    %% gets the source code path for the compiled module.
    SourceCode = proplists:get_value(source, CompileInfo),
    %% gets the last modified value of the source code.
    ScrPreLMT = filelib:last_modified(SourceCode),
    case ScrPreLMT of
        0 ->
            %% NOTE: this means invalid time info, therefore means invalid source
            %% code path; out of date compilation, file was moved from the
            %% original position when compiled.

            %% gets current working directory.
            {ok, CWD} = file:get_cwd(),
            %% converts it to upper.
            UCWD = string:to_upper(CWD),
            %% checks if the invalid meta info is from one of the project
            %% modules, if so, warn it otherwise do nothing.
            case lists:prefix(UCWD, UModBeamPath) of
                true ->
                    io:format(
                        "Module '~p.beam' meta info is out of date, you should clean it and~n"
                        "recompile it if you want dynamic compile to work properly for this module!~n",
                        [Module]),
                    {error, out_of_date_beam};
                false ->
                    ok
            end;
        _ ->
            %% converts the source code last modified time to universal time.
            ScrLastModifiedTime = erlang:localtime_to_universaltime(ScrPreLMT),
            %% compares the last modified time of the source code against the
            %% compiled version.
            case ScrLastModifiedTime > CompileTime of
                true ->
                    %% NOTE: The source code is newest than compiled version,
                    %% so changes where made to the source code and it need
                    %% to be recompiled.
                    case case filename:extension(SourceCode) of
                        Ext when (Ext == ".html") or (Ext == ".dtl") ->
                            compile_template(SourceCode, CompileOpts);
                        _ ->
                            io:format("-------- Compile: ~p  ~p~n", [Module, SourceCode]),
                            compile_file(SourceCode, CompileOpts, UModBeamPath)
                    end of
                        error ->
                            io:format("error!~n"),
                            error;
                        {error, Errors, _} ->
                            io:format("error!~n   ~p", [Errors]),
                            error;
                        _ ->
                            %% NOTE: the compilation was a success.
                            code:purge(Module),
                            code:load_file(Module),
                            io:format("done.~n"),
                            ok
                    end;
                false ->
                    %% NOTE: do nothing because there is no change in the source code.
                    ok
            end
    end.

%% NOTE: the source is from a erlydtl template.
compile_template(SourceCode, CompileOpts) ->
    {ok, CWD} = file:get_cwd(),
    OutDir = filename:join(CWD, "ebin"),
    TN = filename:basename(
        filename:rootname(SourceCode)) ++ "_dtl",
    io:format("  => Reloading modified template: ~p...",
        [filename:basename(SourceCode)]),
    %% compiles the template.
    erlydtl:compile_file(SourceCode, TN,
        CompileOpts ++ [{out_dir, OutDir}, return_errors]).

%% NOTE: the source is a normal erlang file.
compile_file(SourceCode, CompileOpts, BeamPath) ->
    io:format(
        "  => Reloading modified file: ~p...",
        [filename:basename(SourceCode)]),
    %% gets current the module ebin directory.
    CurrOutDir = filename:dirname(BeamPath),
    %% replaces the old metadata '{outdir, OldOutDir}' clause
    %% from compile options with '{outdir, CurrOutDir}'.
    NewCompileOpts =
        lists:keyreplace(outdir, 1, CompileOpts,
            {outdir, CurrOutDir}),
    %% compiles the source file.
    compile:file(SourceCode,
        NewCompileOpts ++ [return_errors]).