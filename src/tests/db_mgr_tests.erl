%%-------------------------------------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2015, Ivanco Software Corporation
%% @doc
%% Tests for 'database_manager'.
%% @end
%% Created : 05. Oct 2015 9:54 AM
%%-------------------------------------------------------------------------------------------------
-module(db_mgr_tests).
-author("Ivan Carmenates Garcia").

%% API
-compile[export_all].

mesure(Fun) when is_function(Fun) ->
    {T, _} = timer:tc(fun() -> Fun() end),
    T / 1000.

test_find(N) ->
    database_manager:connection_block(fun(C) ->
        mesure(fun() ->
            lists:foreach(fun(_) ->
                database_manager:find(C,
                    [roles, users],
                    [[{{users, id}, '==', {roles, id}},
                        {{users, id}, '==', 1}],
                        'or',
                        {{users, id}, '==', 1}],
                    [
                        {return_fields, [{users, id, users_id}, {roles, id, roles_id}]},
                        {limit, 2},
                        {offset, 13}
                    ])
            end, lists:seq(1, N))
        end)
    end, [{backend, main_backend}]).

test_parse_table_names(N) ->
    mesure(fun() ->
        lists:foreach(fun(_) ->
            sql_builder_tools:unparse_table_names([a, b, c, d, e, f], ",")
        end, lists:seq(1, N))
    end).

test_parse_return_simple_fields(N) ->
    mesure(fun() ->
        lists:foreach(fun(_) ->
            sql_builder_tools:unparse_return_simple_fields([a, b, c, d, e, f], ",")
        end, lists:seq(1, N))
    end).

test_parse_return_full_fields(N) ->
    mesure(fun() ->
        lists:foreach(fun(_) ->
            {_, Tabs} = sql_builder_tools:unparse_return_full_fields(
                [{users, '*'}, name, {roles, [id, level]}, {users, name, alias}],
                ",", fun get_postgres_operator/2),
            lists:usort(Tabs)
        end, lists:seq(1, N))
    end).

test_parse_fieldvalues(N) ->
    mesure(fun() ->
        lists:foreach(fun(_) ->
            sql_builder_tools:unparse_fieldvalues(1, #{a=>1, b=>2, c=>"as", d=>4, e=>4, f=>12}, "$")
        end, lists:seq(1, N))
    end).

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