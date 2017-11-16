%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% This is the postgres database backend module.
%%% @end
%%% Created : 12. Aug 2015 5:15 PM
%%%-------------------------------------------------------------------
-module(postgres_backend).
-author("Ivan Carmenates Garcia").

%%-------------------------------------------------------------------------------------------------
%% API Exports
%%-------------------------------------------------------------------------------------------------
-export([
    connect/5,
    disconnect/1,
    query/4,
    transaction/2,
    insert/3,
    update/4,
    delete/4,
    find/4]).

%%%-------------------------------------------------------------------
%%% TYPE Exports
%%%-------------------------------------------------------------------
-type params() :: [] | [any(), ...].

-export_type([params/0]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% @see database_manager:connect/2.
%% @end
%%-------------------------------------------------------------------
-spec connect(Server, Username, Password, Database, Timeout) ->
    {ok, ConRef}
    | {error, closed}
    | {error, Other} when
    Server :: string(),
    Username :: string(),
    Password :: string(),
    Database :: string(),
    Timeout :: timeout(),
    ConRef :: epgsql:connection(),
    Other :: term().
connect(Server, Username, Password, Database, Timeout) ->
    %% Prepares a database connection session.
    {ok, ConRef} = epgsqla:start_link(),
    Ref = epgsqla:connect(ConRef, Server, Username, Password,
        [{database, Database}, {timeout, Timeout}]),
    receive
        {ConRef, Ref, connected} ->
            {ok, ConRef};
        {ConRef, Ref, Error = {error, _}} ->
            debug_logger:log_error(
                erlang:get_stacktrace(),
                "Couldn't connect to the database: ~p", [Error]),
            Error;
        {'EXIT', ConRef, _Reason} ->
            debug_logger:log_error(
                erlang:get_stacktrace(),
                "Database connection closed, ConRef: ~p", [ConRef]),
            {error, closed}
    end.

%%-------------------------------------------------------------------
%% @doc
%% @see database_manager:disconnect/1.
%% @end
%%-------------------------------------------------------------------
-spec disconnect(ConRef) -> ok when
    ConRef :: epgsql:connection().
disconnect(ConRef) ->
    epgsql:close(ConRef).

%%-------------------------------------------------------------------
%% @doc
%% @see database_manager:query/4.
%% @end
%%-------------------------------------------------------------------
-spec query(ConRef, Query, Params, Options) ->
    {ok, database_manager:result_data()} |
    {ok, Count} |
    {ok, Count, database_manager:result_data()}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    ConRef :: epgsql:connection(),
    Query :: string() | bitstring(),
    Params :: [] | [any(), ...],
    Options :: proplists:proplist(),
    Other :: term().
query(ConRef, Query, Params, Options) ->
    case Params of
        [] ->
            Ref = epgsqla:squery(ConRef, Query),
            receive
                {ConRef, Ref, {error, Reason}} ->
                    %% logs errors from 'squery'.
                    debug_logger:log_error(
                        erlang:get_stacktrace(),
                        "DATABASE_SIMPLE_QUERY_ERROR: ~p", [Reason]),
                    {error, Reason};
                {ConRef, Ref, Result} ->
                    log_result(Query, Params, Options, Result)
            end;
        EQParams ->
            case parse(ConRef, Query) of
                {ok, Statement} ->
                    Ref = epgsqla:equery(ConRef, Statement, EQParams),
                    receive
                        {ConRef, Ref, {error, Reason}} ->
                            %% logs errors from 'equery'.
                            debug_logger:log_error(
                                erlang:get_stacktrace(),
                                "DATABASE_QUERY_ERROR: ~p", [Reason]),
                            {error, Reason};
                        {ConRef, Ref, Result} ->
                            log_result(Query, Params, Options, Result)

                    end;
                {error, Reason} ->
                    %% in case of parse error sync is required.
                    epgsql:sync(ConRef),
                    %% logs errors from 'parse'.
                    debug_logger:log_error(
                        erlang:get_stacktrace(),
                        "DATABASE_PARSE_ERROR: ~p", [Reason]),
                    {error, Reason}
            end
    end.

-ifdef(dev_mode).
log_result(Query, Params, Options, RawResult) ->
    Result = to_result_format(RawResult, Options),
    debug_logger:log_info_msg(
        "~n    Query: ~p"
        "~n    Params:~p"
        "~n    Options:~p"
        "~n    RawResult: ~p"
        "~n    Result:~p",
        [Query, Params, Options, RawResult, Result]),
    Result.
-else.
log_result(Query, Params, Options, RawResult) ->
    %% TODO: remove the log later when the dev mode is working.
    Result = to_result_format(RawResult, Options),
    debug_logger:log_info_msg(
        "~n    Query: ~p"
        "~n    Params:~p"
        "~n    Options:~p"
        "~n    RawResult: ~p"
        "~n    Result:~p",
        [Query, Params, Options, RawResult, Result]),
    Result.
-endif.

%%-------------------------------------------------------------------
%% @doc
%% @see database_manager:transaction/2.
%% @end
%%-------------------------------------------------------------------
-spec transaction(ConRef, Fun) ->
    Reply
    | {error, {rollback, Reason}} when
    ConRef :: epgsql:connection(),
    Fun :: fun(),
    Reply :: any(),
    Reason :: term().
transaction(ConRef, Fun) ->
    TR = epgsql:with_transaction(ConRef, fun(_) -> Fun() end),
    case TR of
        {rollback, Reason} ->
            debug_logger:log_error(
                erlang:get_stacktrace(),
                "QUERY_TRANSACTION_ERROR: ~p",
                [Reason]),
            {error, {rollback, Reason}};
        Result ->
            Result
    end.

%%-------------------------------------------------------------------
%% @doc
%% @see database_manager:insert/3.
%% @end
%%-------------------------------------------------------------------
-spec insert(ConRef, TableSpec, Options) ->
    {ok, Count} |
    {ok, Count, database_manager:result_data_for_insert()}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    ConRef :: epgsql:connection(),
    TableSpec :: database_manager:table_spec(),
    Options :: proplists:proplist(),
    Other :: term().
insert(ConRef, {TableName, FieldsMap}, Options) ->
    %% gets field names in string format, i.e.: "id, name, ...".
    FieldNames = sql_builder_tools:unparse_map_fieldnames(FieldsMap, ","),
    %% ---
    %% gets field values and its place holders in string format
    %% i.e.: {"$1, $2", [1, "Test"]}.
    {ValuePlaceHolders, Values, _} =
        sql_builder_tools:unparse_fieldvalues(1, FieldsMap, "$"),
    %% ---
    %% gets the "RETURNING" part of the update sql string.
    MaybeReturningClause = get_return_clause_for_insert_or_delete(Options),
    %% ---
    Query = list_to_bitstring(
        "INSERT INTO " ++ atom_to_list(TableName) ++ " (" ++ FieldNames ++ ")" ++
            " VALUES (" ++ ValuePlaceHolders ++ ")"
            ++ MaybeReturningClause),
    query(ConRef, Query, Values, Options).

%%-------------------------------------------------------------------
%% @doc
%% @see database_manager:update/4.
%% @end
%%-------------------------------------------------------------------
-spec update(ConRef, TableSpec, MatchFieldSpecs, Options) ->
    {ok, Count} |
    {ok, Count, database_manager:result_data()}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    ConRef :: epgsql:connection(),
    TableSpec :: database_manager:table_spec(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    Options :: proplists:proplist(),
    Other :: term().
update(ConRef, {TableName, FieldsMap},
    MatchFieldSpecsMap, Options) when is_map(MatchFieldSpecsMap) ->
    MatchFieldSpecs = maps:to_list(MatchFieldSpecsMap),
    update(ConRef, {TableName, FieldsMap},
        MatchFieldSpecs, Options);
update(ConRef, {TableName, FieldsMap}, MatchFieldSpecs, Options) ->
    %% gets field names in string format, i.e.: "id, name, ...".
    FieldNames = sql_builder_tools:unparse_map_fieldnames(FieldsMap, ","),
    %% ---
    %% gets field values and its place holders in string format
    %% i.e.: {"$1, $2", [1, "Test"]}.
    {ValuePlaceHolders, Values, LastPlaceHolderCount} =
        sql_builder_tools:unparse_fieldvalues(1, FieldsMap, "$"),
    %% ---
    %% gets "WHERE" parameters of the update sql string.
    {WhereParams, WhereValues, SpecsTableNames, _} =
        sql_builder_tools:unparse_match_field_specs(LastPlaceHolderCount,
            MatchFieldSpecs, fun get_postgres_operator/2, true),
    %% ---
    %% gets the "RETURNING" part of the update sql string.
    {MaybeReturningClause, RetTableNames} = get_return_clause_for_update(Options),
    %% ---
    %% deletes the current table name from ExtraTableNames returned by
    %% 'get_where_clause/4' so it will not be included in the "FROM".
    TableNames = lists:delete(TableName, lists:usort(SpecsTableNames ++ RetTableNames)),
    %% gets the "FROM" part of the update sql string.
    MaybeFromClause = get_from_clause(TableNames),
    %% ---
    %% gets the "WHERE" part of the update sql string.
    MaybeWhereClause = get_where_clause(WhereParams),
    %% ---
    %% builds the query string.
    Query = list_to_bitstring(
        "UPDATE " ++ atom_to_list(TableName) ++
            " SET (" ++ FieldNames ++ ") = (" ++ ValuePlaceHolders ++ ")"
            ++ MaybeFromClause
            ++ MaybeWhereClause
            ++ MaybeReturningClause),
    query(ConRef, Query, Values ++ WhereValues, Options).

%%-------------------------------------------------------------------
%% @doc
%% @see database_manager:find/4.
%% @end
%%-------------------------------------------------------------------
-spec find(ConRef, TableNames, MatchFieldSpecs, Options) ->
    {ok, database_manager:result_data()}
    | {error, {invalid_clause_option, term()}}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    ConRef :: epgsql:connection(),
    TableNames :: atom() | [atom(), ...],
    MatchFieldSpecs :: database_manager:condition_term() | database_manager:match_field_specs(),
    Options :: proplists:proplist(),
    Other :: term().
find(ConRef, TableNames, MatchFieldSpecsMap, Options) when is_map(MatchFieldSpecsMap) ->
    MatchFieldSpecs = maps:to_list(MatchFieldSpecsMap),
    find(ConRef, TableNames, MatchFieldSpecs, Options);
find(ConRef, TableNames, MatchFieldSpecs, Options) ->
    %% gets the parameters for the "SELECT" part of the sql string.
    {SelectParams, _TNs, NewOptions} = get_select_params_for_find(Options),
    %% ---
    %% gets the "FROM" part of the delete sql string.
    FromParams = sql_builder_tools:unparse_table_names(TableNames, ","),
    %% ---
    %% gets 'where' parameters of the select sql string.
    {WhereParams, WhereValues, _SpecsTableNames, LastVHPCount} =
        sql_builder_tools:unparse_match_field_specs(1,
            MatchFieldSpecs, fun get_postgres_operator/2, true),
    %% gets the "WHERE" part of the select sql string.
    MaybeWhereClause = get_where_clause(WhereParams),
    %% ---
    %% gets "SELECT" options, "ORDER BY", etc.
    {ParsedClauseOptions, ClauseOptionsValues, _} =
        parse_select_clause_options(NewOptions, LastVHPCount),
    %% ---
    %% builds the query string.
    Query = list_to_bitstring(
        "SELECT " ++ SelectParams ++
            " FROM " ++ FromParams
            ++ MaybeWhereClause
            ++ ParsedClauseOptions),
    query(ConRef, Query, WhereValues ++ ClauseOptionsValues, NewOptions).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% @see database_manager:delete/4.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec delete(ConRef, TableName, MatchFieldSpecs, Options) ->
    {ok, Count} |
    {ok, Count, database_manager:result_data()}
    | {error, {invalid_result_format, term()}}
    | {error, Other} when
    ConRef :: epgsql:connection(),
    TableName :: atom(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    Options :: proplists:proplist(),
    Other :: term().

delete(ConRef, TableName, MatchFieldSpecsMap, Options) when is_map(MatchFieldSpecsMap) ->
    MatchFieldSpecs = maps:to_list(MatchFieldSpecsMap),
    delete(ConRef, TableName, MatchFieldSpecs, Options);
delete(ConRef, TableName, MatchFieldSpecs, Options) when is_atom(TableName) ->
    %% gets "WHERE" parameters of the delete sql string.
    {WhereParams, WhereValues, _, _} =
        sql_builder_tools:unparse_match_field_specs(1,
            MatchFieldSpecs, fun get_postgres_operator/2, false),
    %% gets the "WHERE" part of the delete sql string.
    MaybeWhereClause = get_where_clause(WhereParams),
    %% ---
    %% gets the "RETURNING" part of the delete sql string.
    MaybeReturningClause = get_return_clause_for_insert_or_delete(Options),
    %% ---
    %% gets the "FROM" part of the delete sql string.
    FromClause = get_from_clause(TableName),
    %% ---
    %% builds the delete query string.
    Query = list_to_bitstring(
        "DELETE" ++ FromClause
            ++ MaybeWhereClause
            ++ MaybeReturningClause),
    query(ConRef, Query, WhereValues, Options).

%%-------------------------------------------------------------------------------------------------
%% Internal Functions
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gives the query result depending of 'Options'
%% @end
%%-------------------------------------------------------------------------------------------------
to_result_format(Result, Options) ->
    case lists:keyfind(result_format, 1, Options) of
        false ->
            %% converts query result to a map or list of maps.
            to_map(Result);
        {result_format, map} ->
            %% converts query result to a map or list of maps.
            to_map(Result);
        {result_format, proplist} ->
            %% converts query result to a proplist or list of proplist.
            to_proplist(Result);
        {result_format, raw} ->
            %% give the result in raw format (the same as the driver).
            Result;
        {result_format, Other} ->
            {error, {invalid_result_format, Other}}
    end.

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Returns a map or a list of maps from a database query result.
%% example:
%%     {ok, #{id : Id, name: Name}} = to_map(QueryResult).
%% @end
%%-------------------------------------------------------------------------------------------------
-spec to_map(Result) -> map() | [map(), ...] when
    Result :: {ok, Columns, Rows} | {ok, Count} | {ok, Count, Columns, Rows}.
to_map(Result) ->
    case Result of
        {ok, Count} ->
            {ok, Count};
        {ok, Columns, Rows} ->
            {ok, to_map2(Columns, Rows)};
        {ok, Count, Columns, Rows} ->
            {ok, Count, to_map2(Columns, Rows)}
    end.

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Returns a map from 'Columns' and 'Rows'
%% @end
%%-------------------------------------------------------------------------------------------------
-spec to_map2(Columns, Rows) -> map() when
    Columns :: [{column, Name :: binary(), _, _, _, _}, ...],
    Rows :: [RowData :: tuple(), ...].
to_map2(Columns, Rows) ->
    Maps =
        lists:map(fun(PropList) ->
            AKPL =
                lists:keymap(fun(Key) ->
                    %% converts all keys to atom.
                    binary_to_atom(Key, utf8)
                end, 1, PropList),
            maps:from_list(AKPL)
        end, to_proplist2(Columns, Rows)),
    case length(Maps) of
        0 ->
            no_data;
        1 ->
            erlang:hd(Maps);
        _ ->
            Maps
    end.

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Returns a proplist or a list of proplist from a query result.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec to_proplist(Result) ->
    proplists:proplist() | [proplists:proplist(), ...] when
    Result :: {ok, Columns, Rows} | {ok, Count} | {ok, Count, Columns, Rows}.
to_proplist(Result) ->
    case Result of
        {ok, Count} ->
            {ok, Count};
        {ok, Columns, Rows} ->
            {ok, to_proplist2(Columns, Rows)};
        {ok, Count, Columns, Rows} ->
            {ok, Count, to_proplist2(Columns, Rows)}
    end.

%% @private
to_proplist2(Columns, Rows) ->
    %% gets only a list with the names of the columns.
    ColumnNames = lists:map(
        fun({column, Names, _, _, _, _}) ->
            Names
        end, Columns),
    PropList = lists:map(
        fun(X) ->
            %% adjuncts the value to the column name.
            lists:zip(ColumnNames, tuple_to_list(X))
        end, Rows),
    PropList.

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Parses an sql statement.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec parse(ConRef, Query) ->
    {ok, Statement} | {error, Reason} when
    ConRef :: epgsql:connection(),
    Query :: query_manager:query(),
    Statement :: any(),
    Reason :: term().
parse(ConRef, Query) ->
    Ref = epgsqla:parse(ConRef, Query),
    receive
        {ConRef, Ref, {ok, Statement}} ->
            {ok, Statement};
        {ConRef, Ref, {error, Reason}} ->
            {error, Reason}
    end.

%% @private
-spec get_return_clause_for_insert_or_delete(Options :: proplists:proplist()) ->
    SqlStr :: string().
get_return_clause_for_insert_or_delete(Options) ->
    case lists:keyfind(return_fields, 1, Options) of
        false ->
            "";
        {return_fields, ReturnFields} ->
            case sql_builder_tools:unparse_return_simple_fields(ReturnFields, ",") of
                [] ->
                    "";
                ReturnFieldStr ->
                    " RETURNING " ++ ReturnFieldStr
            end
    end.

%% @private
-spec get_return_clause_for_update(Options :: proplists:proplist()) ->
    {SqlStr :: string(), TableNames :: [atom(), ...]}.
get_return_clause_for_update(Options) ->
    case lists:keyfind(return_fields, 1, Options) of
        false ->
            {[], []};
        {return_fields, ReturnFields} ->
            case sql_builder_tools:unparse_return_full_fields(ReturnFields, ",",
                fun get_postgres_operator/2)
            of
                {[], []} ->
                    "";
                {ReturnFieldStr, TableNames} ->
                    {" RETURNING " ++ ReturnFieldStr, TableNames}
            end
    end.

%% @private
get_from_clause(TableNames) ->
    case TableNames of
        [] ->
            "";
        _ ->
            " FROM " ++ sql_builder_tools:unparse_table_names(TableNames, ",")
    end.

%% @private
get_where_clause(WhereParams) ->
    case WhereParams of
        [] ->
            "";
        _ ->
            " WHERE " ++ WhereParams
    end.

%% @private
-spec get_select_params_for_find(Options :: proplists:proplist()) ->
    {SqlStr :: string(), TableNames :: [atom(), ...]}.
get_select_params_for_find(Options) ->
    case lists:keyfind(return_fields, 1, Options) of
        false ->
            {"*", [], Options};
        {return_fields, ReturnFields} ->
            %%             NewOptions = lists:keydelete(return_fields, 1, Options),
            case sql_builder_tools:unparse_return_full_fields(ReturnFields, ",",
                fun get_postgres_operator/2)
            of
                {[], [], Options} ->
                    {"*", []};
                {ReturnFieldStr, TableNames} ->
                    {ReturnFieldStr, TableNames, Options}
            end
    end.

%% @private
%% TODO: do this better, the order is important!!!.
parse_select_clause_options(Options, VHPCountStart) ->
    OrderByClause =
        case lists:keyfind(order_by, 1, Options) of
            {order_by, OrderBySpecs} ->
                " ORDER BY " ++ parse_order_by_or_group_by_specs(OrderBySpecs);
            false ->
                ""
        end,
    GroupByClause =
        case lists:keyfind(group_by, 1, Options) of
            {group_by, GroupBySpecs} ->
                " GROUP BY " ++ parse_order_by_or_group_by_specs(GroupBySpecs);
            false ->
                ""
        end,
    {LimitClause, LValue, LVHPCount} =
        case lists:keyfind(limit, 1, Options) of
            {limit, LN} ->
                {" LIMIT $" ++ integer_to_list(VHPCountStart), [LN],
                    VHPCountStart + 1};
            false ->
                {"", [], VHPCountStart}
        end,
    {OffsetClause, OValue, OVHPCount} =
        case lists:keyfind(offset, 1, Options) of
            {offset, ON} ->
                {" OFFSET $" ++ integer_to_list(LVHPCount), [ON],
                    LVHPCount + 1};
            false ->
                {"", [], LVHPCount}
        end,
    {OrderByClause ++ GroupByClause ++ LimitClause ++ OffsetClause,
            LValue ++ OValue, OVHPCount}.

parse_order_by_or_group_by_specs(Specs) ->
    "".

%% @private
get_postgres_operator(Operator, Type) ->
    case {Type, Operator} of
        %% match operators.
        {math_op, '=='} -> "=";
        {math_op, 'eq'} -> "=";
        {math_op, 'gt'} -> ">";
        {math_op, '>'} -> ">";
        {math_op, 'lt'} -> "<";
        {math_op, '<'} -> "<";
        {math_op, 'gte'} -> ">=";
        {math_op, '>='} -> ">=";
        {math_op, 'lte'} -> "=<";
        {math_op, '=<'} -> "=<";
        {math_op, 'ne'} -> "/=";
        {math_op, '/='} -> "/=";

        %% special operators.
        {special_op, '('} -> "(";
        {special_op, ')'} -> ")";
        {special_op, '.'} -> ".";
        {special_op, '$'} -> "$";
        {special_op, '*'} -> "*";
        {special_op, 'ALIAS'} -> "AS";

        %% logic operators.
        {logic_op, 'and'} -> "and";
        {logic_op, 'or'} -> "or"
    end.