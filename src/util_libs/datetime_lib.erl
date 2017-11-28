%%-------------------------------------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2015, Ivanco Software Corporation
%% @doc
%% DateTime util module.
%% @end
%% Created : 23. Jul 2015 2:29 AM
%%-------------------------------------------------------------------------------------------------
-module(datetime_lib).
-author("Ivan Carmenates Garcia").

%%-------------------------------------------------------------------------------------------------
%% API Exports
%%-------------------------------------------------------------------------------------------------
-export([
    now_integer/0,
    now_integer_timeout/1,
    integer_to_timestamp/1,
    integer_to_datetime/1,
    timestamp_to_integer/1,
    datetime_to_integer/1,
    encode_date/1,
    decode_date/1]).

%%-------------------------------------------------------------------------------------------------
%% API Functions
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets an integer value for the local server time represented by
%% erlang:system_time(micro_seconds).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec now_integer() -> pos_integer().
now_integer() ->
    erlang:system_time().

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets an integer value for the local server time represented by
%% erlang:system_time(micro_seconds) plus a Timeout.
%%
%% Example: now_integer_timeout(timer:minutes(20)).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec now_integer_timeout(Timeout) -> integer() when
    Timeout :: pos_integer().
now_integer_timeout(Timeout) ->
    now_integer() + Timeout * 1000.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets the timestamp {MegaSecs, Secs, MicroSecs} format from an
%% integer obtained from erlang:system_time(micro_seconds) or
%% now_integer/0 function.
%%
%% Example: integer_to_timestamp(now_integer()).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec integer_to_timestamp(Integer) -> erlang:timestamp() when
    Integer :: pos_integer().
integer_to_timestamp(Integer) ->
    MegaSecs = Integer div 1000000000000,
    Secs = Integer div 1000000 - MegaSecs * 1000000,
    MicroSecs = Integer rem 1000000,
    {MegaSecs, Secs, MicroSecs}.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets the datetime tuple {date(), time()} from an integer obtained
%% from erlang:system_time(micro_seconds) or now_integer/0 function.
%%
%% Example: integer_to_datetime(now_integer()).
%% Example of result: {{2016,5,3},{18,51,2}}
%% @end
%%-------------------------------------------------------------------------------------------------
-spec integer_to_datetime(Integer) -> calendar:datetime() when
    Integer :: pos_integer().
integer_to_datetime(Integer) ->
    calendar:now_to_datetime(integer_to_timestamp(Integer)).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets the integer representation of time like in
%% erlang:system_time(micro_seconds) from a timestamp
%% {MegaSecs, Secs, MicroSecs} format.
%%
%% Example: timestamp_to_integer({1462,233954,0}).
%% Example of result: 1462302341418289
%% @end
%%-------------------------------------------------------------------------------------------------
-spec timestamp_to_integer(TimeStamp) -> integer() when
    TimeStamp :: erlang:timestamp().
timestamp_to_integer({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs div 1000.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Gets the an integer representation of time same as returned by
%% erlang:system_time(micro_seconds) or now_integer/0 function.
%%
%% Example: datetime_to_integer(calendar:universal_time()).
%% Example of result: 1462302341418289
%% @end
%%-------------------------------------------------------------------------------------------------
-spec datetime_to_integer(DateTime) -> pos_integer when
    DateTime :: calendar:datetime().
datetime_to_integer({Date, Time} = DateTime) when is_tuple(Date), is_tuple(Time) ->
    %% TODO: implement it better, using gregorian secs maybe?
    FD = dh_date:format("d-M-Y h:m:s", DateTime),
    dh_date:nparse(FD),
    timestamp_to_integer(FD).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Encodes any possible Erlang form of representation of time into a
%% database ready format.
%%
%% Example: encode_date(calendar:universal_time()).
%% Example: encode_date(erlang:timestamp()).
%% Example: encode_date(erlang:system_time(micro_seconds)).
%% Example of result: "03-May-2016 00:05:54"
%% @end
%%-------------------------------------------------------------------------------------------------
-spec encode_date(TimeRepresentation) -> string() when
    TimeRepresentation :: calendar:datetime() | erlang:timestamp() | pos_integer().
encode_date({MegaSecs, Secs, MicroSec} = TimeStamp) when
    is_integer(MegaSecs), is_integer(Secs), is_integer(MicroSec) ->
    dh_date:format("d-M-Y h:m:s", TimeStamp);
encode_date({Date, Time} = DateTime) when is_tuple(Date), is_tuple(Time) ->
    dh_date:format("d-M-Y h:m:s", DateTime);
encode_date(Number) when is_integer(Number) ->
    TimeStamp = integer_to_timestamp(Number),
    encode_date(TimeStamp).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Decodes the result from the database to an Erlang datetime tuple.
%%
%% Example: decode_date(DBResult).
%% Example of result: "03-May-2016 00:05:54"
%% @end
%%-------------------------------------------------------------------------------------------------
-spec decode_date(EncodedDate) -> calendar:datetime() when
    EncodedDate :: calendar:datetime() | string() | binary().
decode_date({Date, Time} = DateTime) when is_tuple(Date), is_tuple(Time) ->
    DateTime;
decode_date(Binary) when is_binary(Binary) ->
    decode_date(binary_to_list(Binary));
decode_date(String) when is_list(String) ->
    dh_date:parse(String).