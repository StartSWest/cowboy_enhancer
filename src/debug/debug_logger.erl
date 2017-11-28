%%-------------------------------------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2017, Ivanco Software Corporation
%% @doc
%%
%% @end
%% Created: 15. Jul 2015 10:43 PM
%%-------------------------------------------------------------------------------------------------
-module(debug_logger).
-author("Ivan Carmenates Garcia").

%%=================================================================================================
%% API Exports
%%=================================================================================================
-export([
  log_error/2,
  log_error/3,
  error_msg/1,
  error_msg/2,
  warning_msg/1,
  warning_msg/2,
  info_msg/1,
  info_msg/2,
  debug/1,
  debug/2]).

%%=================================================================================================
%% API Functions
%%=================================================================================================

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Logs an error and stack trace.
%% NOTE: 'Params' is the same as in io:format("~p", [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec log_error(StackTrace, Params)
  -> ok
   | {error, Reason}
  when
    StackTrace :: term(),
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
log_error(StackTrace, Params) ->
  log_error(StackTrace, "~p", [Params]).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Logs an error and stack trace.
%% NOTE: 'ErrorFormat' and 'Params' is the same as in io:format(ErrorFormat, [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec log_error(StackTrace, ErrorFormat, Params)
  -> ok
   | {error, Reason}
  when
    StackTrace :: term(),
    ErrorFormat :: string(),
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
log_error(StackTrace, ErrorFormat, Params) ->
  try
    error(log_error)
  catch
    error:log_error ->
      error_logger:error_msg(
        "\n==================== Debug Logger! ====================\n"
        "  ERROR: " ++ ErrorFormat ++ "\n  StackTrace: ~p~n",
        Params ++ [StackTrace])
  end.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Logs an error message.
%% NOTE: 'Params' is the same as in io:format("~p", [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec error_msg(Params)
  -> ok
   | {error, Reason}
  when
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
error_msg(Params) ->
  error_msg("~p", [Params]).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Logs an error message.
%% NOTE: 'ErrorFormat' and 'Params' is the same as in io:format(ErrorFormat, [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec error_msg(ErrorFormat, Params)
  -> ok
   | {error, Reason}
  when
    ErrorFormat :: string(),
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
error_msg(ErrorFormat, Params) ->
  error_logger:error_msg(
    "\n==================== Debug Logger! ====================\n"
    "  ERROR: " ++ ErrorFormat ++ "~n", Params).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Logs a warning message.
%% NOTE: 'Params' is the same as in io:format("~p", [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec warning_msg(Params)
  -> ok 
  | {error, Reason}
  when
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
warning_msg(Params) ->
  warning_msg("~p", [Params]).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Logs a warning message.
%% NOTE: 'WarningFormat' and 'Params' is the same as in io:format(WarningFormat, [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec warning_msg(WarningFormat, Params)
  -> ok
   | {error, Reason}
  when
    WarningFormat :: string(),
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
warning_msg(WarningFormat, Params) ->
  error_logger:warning_msg(
    "\n==================== Debug Logger! ====================\n"
    "  WARNING: " ++ WarningFormat ++ "~n", Params).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Logs a info message.
%% NOTE: 'Params' is the same as in io:format("~p", [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec info_msg(Params)
  -> ok
   | {error, Reason}
  when
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
info_msg(Params) ->
  info_msg("~p", [Params]).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Logs a info message.
%% NOTE: 'MessageFormat' and 'Params' is the same as in io:format(MessageFormat, [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec info_msg(MessageFormat, Params)
  -> ok
   | {error, Reason}
  when
    MessageFormat :: string(),
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
info_msg(MessageFormat, Params) ->
  io:format(
    "\n==================== Debug Logger! ====================\n"
    "  MESSAGE: " ++ MessageFormat ++ "~n", Params).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Shows debug message at it is.
%% NOTE: 'Params' is the same as in io:format("~p", [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec debug(Params)
  -> ok
   | {error, Reason}
  when
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
debug(Params) ->
  debug("~p", [Params]).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Shows debug message at it is.
%% NOTE: 'MessageFormat' and 'Params' is the same as in io:format(MessageFormat, [Params]).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec debug(MessageFormat, Params)
  -> ok
   | {error, Reason}
  when
    MessageFormat :: string(),
    Params :: term(),
    Reason :: term().
%%/////////////////////////////////////////////////////////////////////////////////////////////////
debug(MessageFormat, Params) ->
  io:format(MessageFormat , Params).