%%-------------------------------------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2015, Ivanco Software Corporation
%%
%% @doc
%% Control module for hot code rebuild and load.
%% @end
%% Created : 24. Aug 2015 10:59 AM
%%-------------------------------------------------------------------------------------------------
-module(ce_control).
-author("Ivan Carmenates Garcia").

%%-------------------------------------------------------------------------------------------------
%% API Exports
%%-------------------------------------------------------------------------------------------------
-export([
  rebuild_all/0,
  rebuild/1]).


%%-------------------------------------------------------------------------------------------------
%% API Functions
%%-------------------------------------------------------------------------------------------------

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
  L = [rebuild(M) || {M, F} <- All, is_list(F), not code:is_sticky(M)],

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
      io:format("~nWe have some errors!~n"),
      %---------------
      error
  end.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Rebuilds and loads a recently modified module or erlydtl template.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec rebuild(Module)
  -> ok
   | {warn, out_of_date_beam}
   | ignore
   | {error, Errors, Warnings}
  when
    Module :: atom(),
    Errors :: list(),
    Warnings :: list().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
rebuild(Module) ->
  % gets the module beam path.
  BeamPath = code:which(Module),
  
  % ensures that a module is a compiled one.
  case BeamPath of
    [] ->
      % not compiled so it get ignored.
      ok;
    _ ->
      % only compile files that belongs to the project.
      case check_compilable(BeamPath) of
        true ->
          rebuild2(Module, BeamPath);
        _ ->
          ignore
      end
  end.

%%-------------------------------------------------------------------------------------------------
%% Internal Functions
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
rebuild2(Module, BeamPath)->
  % gets last modified time of the module compiled beam file.
  BeamLMTime = filelib:last_modified(BeamPath),

  case check_last_modified_Time(BeamLMTime, Module, BeamPath) of
    ok ->
      % gets compile info from the module compiled beam.
      CompileInfo = Module:module_info(compile),

      % gets the source code path of the compiled module.
      SourcePath = proplists:get_value(source, CompileInfo),

      % gets last modified time of the source code.
      ScLMTime = filelib:last_modified(SourcePath),
      
      case check_last_modified_Time(ScLMTime, Module, BeamPath) of
        ok ->
          rebuild3(Module, BeamPath, SourcePath, BeamLMTime, ScLMTime, CompileInfo);
        Other ->
          % {warn, out_of_date_beam} | ignore | error.
          Other
      end;
    Other->
      % {warn, out_of_date_beam} | ignore | error.
      Other
  end.

%%-------------------------------------------------------------------------------------------------
rebuild3(Module, BeamPath, SourcePath, BeamLMTime, ScLMTime, CompileInfo) ->
  % gets both beam and source code modified universal time.
  ScLMTime2 = erlang:localtime_to_universaltime(ScLMTime),
  BeamLMTime2 = erlang:localtime_to_universaltime(BeamLMTime),
         
  case ScLMTime2 > BeamLMTime2 of
    true ->
      % code changed need to recompile.
      rebuild4(Module, BeamPath, SourcePath, CompileInfo);
    _ ->
      % code updated nothing to do!
      ok
  end.

%%-------------------------------------------------------------------------------------------------
rebuild4(Module, BeamPath, SourcePath, CompileInfo) ->
  % gets the compile options.
  CompileOpts = proplists:get_value(options, CompileInfo),

  case rebuild5(BeamPath, SourcePath, CompileOpts) of
    error ->
      error;
    {error, Errors, Warnings} ->
      {error, Errors, Warnings};
    _ ->
      % NOTE: the compilation was a success.
      code:purge(Module),
      code:load_file(Module),
      
      %---------------
      % Debug.       -
      %---------------
      io:format("     done.~n"),
      %---------------
      ok
  end.

%%-------------------------------------------------------------------------------------------------
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
compile_template(SourcePath, CompileOpts) ->
  % gets current ebin dir.
  {ok, CWD} = file:get_cwd(), 
  OutDir = filename:join(CWD, "ebin"),

  % gets template file name.
  TN = filename:basename(filename:rootname(SourcePath)) ++ "_dtl",

  %---------------
  % Debug.       -
  %---------------
  io:format("~n  => Recompiling modified template: ~p... ", [filename:basename(SourcePath)]),
  %---------------
  
  % compiles the template.
  erlydtl:compile_file(SourcePath, TN, [{out_dir, OutDir}, return_errors] ++ CompileOpts).

%% NOTE: the source is a normal erlang file.
%%-------------------------------------------------------------------------------------------------
compile_file(BeamPath, SourcePath, CompileOpts) ->
  %---------------
  % Debug.       -
  %---------------
  io:format("~n  => Recompiling modified file: ~p...~n", [filename:basename(SourcePath)]),
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
check_last_modified_Time(LMTime, Module, BeamPath)->
  case LMTime of
    0 ->
      % NOTE: this means invalid time info, therefore means invalid source code path; out of 
      %       date compilation, file was moved from the original position when compiled.
      case check_compilable(BeamPath) of
        true ->
          %---------------
          % Debug.       -
          %---------------
          io:format(
            "~nWarning: Module '~p.beam' meta info is out of date or file doesn't exists. Please ~n"
            "         recompile it for automatic compile to work properly in dev mode for this module!~n",
            [Module]),
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
check_compilable(BeamPath) ->
  % gets current working directory.
  {ok, CWD} = file:get_cwd(),

  % gets if the beam belongs to the project modules.
  lists:prefix(string:to_upper(CWD), string:to_upper(BeamPath)).