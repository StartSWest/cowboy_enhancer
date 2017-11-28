%%-------------------------------------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2017, Ivanco Software Corporation
%%
%% @doc
%% Control module for hot code rebuild and load.
%% @end
%% Created : 24. Aug 2015 10:59 AM
%%-------------------------------------------------------------------------------------------------
-module(ce_control).
-author("Ivan Carmenates Garcia").

%%=================================================================================================
%% API Exports
%%=================================================================================================
-export([
  rebuild_all/0,
  rebuild/1]).

%%=================================================================================================
%% API Functions
%%=================================================================================================

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Rebuilds and loads all recently modified files. This is the way to update the system once it is
%% running.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec rebuild_all()
  -> ok
   | error.
%%/////////////////////////////////////////////////////////////////////////////////////////////////
rebuild_all() ->
  % tries to rebuild all non-sticky recently modified project files.
  All = code:all_loaded(),
  L = [rebuild1(M) || {M, F} <- All, is_list(F), not code:is_sticky(M)],

  % if all files compiled ok, return ok, otherwise error.
  % NOTE: {warning, out_of_date_beam} isn't considered an error.
  case lists:all(
    fun(X) ->
      (ok == X) or ({warn, out_of_date_beam} == X) or (ignore == X)
    end, L)
  of
    true ->
      ok;
    false ->
      %---------------
      % Debug.       -
      %---------------
      debug_logger:debug("~nWe have some errors!~n"),
      %---------------
      error
  end.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Rebuilds and loads a recently modified module or erlydtl template.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec rebuild(ModuleName)
  -> ok
   | {warn, out_of_date_beam}
   | ignore
   | {error, Errors, Warnings}
    | error
  when
    ModuleName :: atom(),
    Errors :: list(),
    Warnings :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
rebuild(ModuleName) ->
  rebuild1(ModuleName).

%%=================================================================================================
%% Internal Functions
%%=================================================================================================

%%-------------------------------------------------------------------------------------------------
-spec rebuild1(ModuleName)
  -> ok
   | {warn, out_of_date_beam}
   | ignore
   | {error, Errors, Warnings}
   | error
  when
    ModuleName :: atom(),
    Errors :: list(),
    Warnings :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
rebuild1(ModuleName)->
  % gets the module beam path.
  BeamPath = code:which(ModuleName),
  % ensures that a module is a compiled one.
  case BeamPath of
    [] ->
      % not compiled so it get ignored.
      ok;
    _ ->
      % only compile files that belongs to the project.
      case check_compilable(BeamPath) of
        true ->
          rebuild2(ModuleName, BeamPath);
        _ ->
          ignore
      end
  end.

%%-------------------------------------------------------------------------------------------------
-spec rebuild2(ModuleName, BeamPath)
  -> ok
   | {warn, out_of_date_beam}
   | ignore
   | {error, Errors, Warnings}
   | error
  when
    ModuleName :: atom(),
    BeamPath :: string(),
    Errors :: list(),
    Warnings :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
rebuild2(ModuleName, BeamPath)->
  % gets last modified time of the module compiled beam file.
  BeamLMTime = filelib:last_modified(BeamPath),

  case check_last_modified_Time(BeamLMTime, ModuleName, BeamPath) of
    ok ->
      % gets compile info from the module compiled beam.
      CompileInfo = ModuleName:module_info(compile),

      % gets the source code path of the compiled module.
      SourcePath = proplists:get_value(source, CompileInfo),

      % gets last modified time of the source code.
      ScLMTime = filelib:last_modified(SourcePath),
      
      case check_last_modified_Time(ScLMTime, ModuleName, BeamPath) of
        ok ->
          rebuild3(ModuleName, BeamPath, SourcePath, BeamLMTime, ScLMTime, CompileInfo);
        Other ->
          % {warn, out_of_date_beam} | ignore | error.
          Other
      end;
    Other->
      % {warn, out_of_date_beam} | ignore | error.
      Other
  end.

%%-------------------------------------------------------------------------------------------------
-spec rebuild3(ModuleName, BeamPath, SourcePath, BeamLMTime, ScLMTime, CompileInfo)
  -> ok
   | {error, Errors, Warnings}
   | error
  when
    ModuleName :: atom(),
    BeamPath :: string(),
    SourcePath :: string(),
    BeamLMTime :: calendar:datetime(),
    ScLMTime :: calendar:datetime(),
    CompileInfo :: proplists:proplist(),
    Errors :: list(),
    Warnings :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
rebuild3(ModuleName, BeamPath, SourcePath, BeamLMTime, ScLMTime, CompileInfo) ->
  % gets both beam and source code modified universal time.
  ScLMTime2 = erlang:localtime_to_universaltime(ScLMTime),
  BeamLMTime2 = erlang:localtime_to_universaltime(BeamLMTime),
         
  case ScLMTime2 > BeamLMTime2 of
    true ->
      % code changed need to recompile.
      rebuild4(ModuleName, BeamPath, SourcePath, CompileInfo);
    _ ->
      % code updated nothing to do!
      ok
  end.

%%-------------------------------------------------------------------------------------------------
-spec rebuild4(ModuleName, BeamPath, SourcePath, CompileInfo)
  -> ok
   | {error, Errors, Warnings}
   | error
  when
    ModuleName :: atom(),
    BeamPath :: string(),
    SourcePath :: string(),
    CompileInfo :: proplists:proplist(),
    Errors :: list(),
    Warnings :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
rebuild4(ModuleName, BeamPath, SourcePath, CompileInfo) ->
  % gets the compile options.
  CompileOpts = proplists:get_value(options, CompileInfo),

  case rebuild5(BeamPath, SourcePath, CompileOpts) of
    error ->
      error;
    {error, Errors, Warnings} ->
      {error, Errors, Warnings};
    _ ->
      % NOTE: the compilation was a success.
      code:purge(ModuleName),
      code:load_file(ModuleName),
      
      %---------------
      % Debug.       -
      %---------------
      debug_logger:debug("     done.~n"),
      %---------------
      ok
  end.

%%-------------------------------------------------------------------------------------------------
-spec rebuild5(BeamPath, SourcePath, CompileOpts)
  -> {ok, ModuleName}
   | {ok, ModuleName, Warnings}
   | {error, Errors, Warnings}
    | error
  when
    BeamPath :: string(),
    SourcePath :: string(),
    CompileOpts :: proplists:proplist(),
    ModuleName :: atom(),
    Warnings :: list(),
    Errors :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
rebuild5(BeamPath, SourcePath, CompileOpts) ->
  case filename:extension(SourcePath) of
    Ext when (Ext == ".html") or (Ext == ".dtl") ->
      % compiles erlydtl template.
      compile_template(SourcePath, CompileOpts);
    _ ->
      % compiles erlang source code file.
      compile_file(BeamPath, SourcePath, CompileOpts)
  end.

%% NOTE: the source is from a erlydtl template.
%%-------------------------------------------------------------------------------------------------
-spec compile_template(SourcePath, CompileOpts)
  -> {ok, ModuleName}
   | error
  when
    SourcePath :: string(),
    CompileOpts :: proplists:proplist(),
    ModuleName :: atom().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
compile_template(SourcePath, CompileOpts) ->
  % gets current ebin dir.
  {ok, CWD} = file:get_cwd(), 
  OutDir = filename:join(CWD, "ebin"),

  % gets template file name.
  TN = filename:basename(filename:rootname(SourcePath)) ++ "_dtl",

  %---------------
  % Debug.       -
  %---------------
  debug_logger:debug(
    "~n  => Recompiling modified template: ~p... ", [filename:basename(SourcePath)]),
  %---------------
  
  % compiles the template.
  erlydtl:compile_file(SourcePath, TN, [{out_dir, OutDir}, return_errors] ++ CompileOpts).

%% NOTE: the source is a normal erlang file.
%%-------------------------------------------------------------------------------------------------
-spec compile_file(BeamPath, SourcePath, CompileOpts)
  -> {ok, ModuleName}
   | {ok, ModuleName, Warnings}
   | {error, Errors, Warnings}
    | error
  when
    BeamPath :: string(),
    SourcePath :: string(),
    CompileOpts :: proplists:proplist(),
    ModuleName :: atom(),
    Warnings :: list(),
    Errors :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
compile_file(BeamPath, SourcePath, CompileOpts) ->
  %---------------
  % Debug.       -
  %---------------
  debug_logger:debug("~n  => Recompiling modified file: ~p...", [filename:basename(SourcePath)]),
  %---------------

  % creates a temp directory for hot compilations.
  TempDir = "/Temp",

  % gets app base current dir.
  {ok, CWD} = file:get_cwd(), 
  % gets the full path where the new compiled beam will be created.
  TempBeamPath = filename:absname_join(CWD ++ "/" ++ TempDir ++ "/", filename:basename(BeamPath)),
  
  % ensures the temp beam output directory exists.
  ok = filelib:ensure_dir(TempBeamPath),
  
  % gets only the dir path without the file name.
  TempBeamDir = filename:dirname(TempBeamPath),
  % replaces the old metadata '{outdir, OldOutDir}' clause from compile options with
  % '{outdir, BeamDir}'.
  NewCompileOpts = lists:keyreplace(outdir, 1, CompileOpts, {outdir, TempBeamDir}),

  % compiles the source file.
  case
    compile:file(SourcePath, [return, report] ++ NewCompileOpts)
  of
    R when element(1, R) == ok ->
      % deletes the old compiled beam file.
      ok = file:delete(BeamPath),
      % moves recently compiled beam file in temp dir to app ebin dir.
      ok = file:rename(TempBeamPath, BeamPath);
    R ->
      R
  end.

%%-------------------------------------------------------------------------------------------------
-spec check_last_modified_Time(LMTime, ModuleName, BeamPath)
  -> ok
   | ignore
   | {warn, out_of_date_beam}
  when
    LMTime :: calendar:datetime(),
    ModuleName :: atom(),
    BeamPath :: string().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
check_last_modified_Time(LMTime, ModuleName, BeamPath)->
  case LMTime of
    0 ->
      % NOTE: this means invalid time info, therefore means invalid source code path; out of 
      %       date compilation, file was moved from the original position when compiled.
      case check_compilable(BeamPath) of
        true ->
          %---------------
          % Debug.       -
          %---------------
          debug_logger:debug(
            "~nWarning: Module '~p.beam' meta info is out of date or file doesn't exists. Please recompile it manually otherwise auto-compile will not work for this module!~n", [ModuleName]),
          %---------------
          {warn, out_of_date_beam};
        false ->
          % get ignored since the module is not from the project.
          ignore
      end;
    _Valid ->
      % Time info is valid.
      ok
  end.

%%-------------------------------------------------------------------------------------------------
-spec check_compilable(BeamPath)
  -> true
   | false
  when
    BeamPath :: string().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
check_compilable(BeamPath) ->
  % gets current working directory.
  {ok, CWD} = file:get_cwd(),

  % gets if the beam belongs to the project modules.
  lists:prefix(string:to_upper(CWD), string:to_upper(BeamPath)).