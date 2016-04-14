%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% DateTime util module.
%%% @end
%%% Created : 23. Jul 2015 2:29 AM
%%%-------------------------------------------------------------------
-module(datetime_lib).
-author("Ivan Carmenates Garcia").

%% -------------------------------------------------------------------
%% API Exports
%% -------------------------------------------------------------------
-export([
    universal_datetime/0,
    datetime_to_string/1,
    now_integer/0,
    now_integer_timeout/1]).

%% -------------------------------------------------------------------
%% API Functions
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc
%% Gets the current universal date and time in a valid string format
%% ready to be inserted in the database.
%% @end
%% -------------------------------------------------------------------
-spec universal_datetime() ->
    string().
universal_datetime() ->
    dh_date:format("d-M-Y h:m:s", calendar:universal_time()).

%% -------------------------------------------------------------------
%% @doc
%% Converts {date(), time()} format to a valid string format ready
%% to be inserted in the database.
%% @end
%% -------------------------------------------------------------------
-spec datetime_to_string(DateTime) -> string() when
    DateTime :: calendar:datetime().
datetime_to_string(DateTime) ->
    dh_date:format("d-M-Y h:m:s", DateTime).

%% -------------------------------------------------------------------
%% @doc
%% Gets an integer value for the local server now.
%% @end
%% -------------------------------------------------------------------
-spec now_integer() -> integer().
now_integer() ->
    erlang:monotonic_time().

%% -------------------------------------------------------------------
%% @doc
%% Gets an integer value for the local server now plus a Timeout.
%% Example: now_integer_timeout(timer:minutes(20)).
%% @end
%% -------------------------------------------------------------------
-spec now_integer_timeout(Timeout) -> integer() when
    Timeout :: integer().
now_integer_timeout(Timeout) ->
    now_integer() + Timeout * 1000.