%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%%
%%% @end
%%% Created : 15. Jul 2015 10:43 PM
%%%-------------------------------------------------------------------
-module(debug_logger).
-author("Ivan Carmenates Garcia").

%% -------------------------------------------------------------------
%% API Exports
%% -------------------------------------------------------------------
-export([
    log_error/2,
    log_error/3,
    log_error_msg/1,
    log_error_msg/2,
    log_warning_msg/1,
    log_warning_msg/2,
    log_info_msg/1,
    log_info_msg/2]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc
%% Logs an error and stack trace.
%% NOTE: 'Params' is the same as io:format("~p", [Params]).
%% @end
%% -------------------------------------------------------------------
-spec log_error(StackTrace, Params) -> ok | {error, Reason} when
    StackTrace :: term(),
    Params :: term(),
    Reason :: term().
log_error(StackTrace, Params) ->
    log_error(StackTrace, "~p", [Params]).

%% -------------------------------------------------------------------
%% @doc
%% Logs an error and stack trace.
%% NOTE: 'ErrorFormat' and 'Params' is the same as:
%%       io:format(ErrorFormat, [Params]).
%% @end
%% -------------------------------------------------------------------
-spec log_error(StackTrace, ErrorFormat, Params) -> ok | {error, Reason} when
    StackTrace :: term(),
    ErrorFormat :: string(),
    Params :: term(),
    Reason :: term().
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

%% -------------------------------------------------------------------
%% @doc
%% Logs an error message without stack trace.
%% NOTE: 'Params' is the same as io:format("~p", [Params]).
%% @end
%% -------------------------------------------------------------------
-spec log_error_msg(Params) -> ok | {error, Reason} when
    Params :: term(),
    Reason :: term().
log_error_msg(Params) ->
    log_error_msg("~p", [Params]).

%% -------------------------------------------------------------------
%% @doc
%% Logs an error message without stack trace.
%% NOTE: 'ErrorFormat' and 'Params' is the same as:
%%       io:format(ErrorFormat, [Params]).
%% @end
%% -------------------------------------------------------------------
-spec log_error_msg(ErrorFormat, Params) -> ok | {error, Reason} when
    ErrorFormat :: string(),
    Params :: term(),
    Reason :: term().
log_error_msg(ErrorFormat, Params) ->
    error_logger:error_msg(
        "\n==================== Debug Logger! ====================\n"
        "  ERROR: " ++ ErrorFormat ++ "~n", Params).

%% -------------------------------------------------------------------
%% @doc
%% Logs a warning message.
%% NOTE: 'Params' is the same as io:format("~p", [Params]).
%% @end
%% -------------------------------------------------------------------
-spec log_warning_msg(Params) -> ok | {error, Reason} when
    Params :: term(),
    Reason :: term().
log_warning_msg(Params) ->
    log_warning_msg("~p", [Params]).

%% -------------------------------------------------------------------
%% @doc
%% Logs a warning message.
%% NOTE: 'WarningFormat' and 'Params' is the same as:
%%       io:format(WarningFormat, [Params]).
%% @end
%% -------------------------------------------------------------------
-spec log_warning_msg(WarningFormat, Params) -> ok | {error, Reason} when
    WarningFormat :: string(),
    Params :: term(),
    Reason :: term().
log_warning_msg(WarningFormat, Params) ->
    error_logger:warning_msg(
        "\n==================== Debug Logger! ====================\n"
        "  WARNING: " ++ WarningFormat ++ "~n", Params).

%% -------------------------------------------------------------------
%% @doc
%% Logs a info message.
%% NOTE: 'Params' is the same as io:format("~p", [Params]).
%% @end
%% -------------------------------------------------------------------
-spec log_info_msg(Params) -> ok | {error, Reason} when
    Params :: term(),
    Reason :: term().
log_info_msg(Params) ->
    log_info_msg("~p", [Params]).

%% -------------------------------------------------------------------
%% @doc
%% Logs a info message.
%% NOTE: 'MessageFormat' and 'Params' is the same as:
%%       io:format(MessageFormat, [Params]).
%% @end
%% -------------------------------------------------------------------
-spec log_info_msg(MessageFormat, Params) -> ok | {error, Reason} when
    MessageFormat :: string(),
    Params :: term(),
    Reason :: term().
log_info_msg(MessageFormat, Params) ->
    io:format(
        "\n==================== Debug Logger! ====================\n"
        "  MESSAGE: " ++ MessageFormat ++ "~n", Params).