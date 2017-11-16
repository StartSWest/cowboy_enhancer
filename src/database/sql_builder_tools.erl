%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% Ports many helper sql string builder functionalities.
%%% @end
%%% Created : 21. Sep 2015 11:03 PM
%%%-------------------------------------------------------------------
-module(sql_builder_tools).
-author("Ivan Carmenates Garcia").

%% API
-export([
    unparse_match_field_specs/4,
    unparse_map_fieldnames/2,
    unparse_table_names/2,
    unparse_return_simple_fields/2,
    unparse_return_full_fields/3,
    unparse_fieldvalues/3]).

-compile([export_all]).

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the parameters for a ` "WHERE" ' clause of a sql query string.
%%
%% Example:
%% <pre>
%%   unparse_match_field_specs(1, [
%%       [{name, '==', "john"}, {password, 'eq', "server*-+"}],
%%       'or',
%%       {{user_profile, age}, '>', 30}],
%%           fun postgres_backend:get_postgres_operator/2, true) ->
%%       %% this example will produce:
%%       %% -- >
%%       {"(name=$1 and password=$2) or user_profile.age>$3",
%%           ["john","server*-+",30], [user_profile], 4}
%% </pre>
%% Note: operator `and' is implicit when two term are separated by
%%       comma.
%%
%% If no `MatchFieldSpecs' is specified the result will be:
%% `{[], [], [], PlaceHolderCountStart}'.
%% @throws {error, invalid_match_field_spec, InvalidForm :: any()} |
%%         {error, invalid_match_simple_field_spec, InvalidForm :: any()}
%% @end
%%-------------------------------------------------------------------------------------------------
-spec unparse_match_field_specs(
    PHCountStart, MatchFieldSpecs, OperatorFinder, AllowFullFieldNames) ->
    {SqlStr, Values, ExtraTableNames, LastCountValuePlaceHolder} when
    PHCountStart :: pos_integer(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    OperatorFinder :: fun(),
    AllowFullFieldNames :: boolean(),
    SqlStr :: string(),
    Values :: list(),
    ExtraTableNames :: [atom(), ...],
    LastCountValuePlaceHolder :: pos_integer().
unparse_match_field_specs(PHCountStart, MatchFieldSpecs, OperatorFinder, AllowFullFieldNames) ->
    {Tokens, Values, TableNames, PHCount} =
        match_field_specs_sql_builder(MatchFieldSpecs,
            [], [], [], PHCountStart, [], OperatorFinder, AllowFullFieldNames),
    {Tokens, Values, TableNames, PHCount}.

%% @private
-spec match_field_specs_sql_builder(MatchSpecs,
    Tokens, Values, TableNames, PHCount, MaybeAndOp, OperatorFinder, AllowFullFieldNames) ->
    {Tokens, Values, TableNames, PHCount} when
    MatchSpecs :: database_manager:match_field_specs(),
    Tokens :: list(),
    Values :: list(),
    TableNames :: [atom(), ...],
    PHCount :: pos_integer(),
    MaybeAndOp :: string() | [],
    OperatorFinder :: fun(),
    AllowFullFieldNames :: boolean().
%% base case.
match_field_specs_sql_builder([], Tokens, Values, TableNames, PHCount, _, _OperatorFinder, _) ->
    {Tokens, Values, TableNames, PHCount};
%% when is group [...] -> (...).
match_field_specs_sql_builder([[_ | _] = MatchSpecs | Rest],
    Tokens, Values, TableNames, PHCount, LogicOp, OperatorFinder, AFFN) ->
    %% -->
    %% opens a group "(",
    %% also an 'and' operator is optionally attached if this is the next
    %% group separated by comma. i.e:
    %% [[...], [this]] -> "(...) and (this)".
    StartGroup = LogicOp ++ OperatorFinder('(', special_op),
    %% loop in inner list (content inside the brackets).
    {Tokens2, Values2, TableNames2, PHCount2} =
        match_field_specs_sql_builder(MatchSpecs,
            StartGroup, Values, TableNames, PHCount, [], OperatorFinder, AFFN),
    %% closes the group ")".
    EndGroup = OperatorFinder(')', special_op),
    %% keep looping through the rest of the match spec.
    match_field_specs_sql_builder(Rest,
        Tokens ++ Tokens2 ++ EndGroup,
        Values2, TableNames2, PHCount2,
        " " ++ OperatorFinder('and', logic_op) ++ " ", OperatorFinder, AFFN);
%% when is [{Term, Term}, ...].
match_field_specs_sql_builder([{Term1, Term2} | Rest],
    Tokens, Values, TableNames, PHCount, LogicOp, OperatorFinder, AFFN) ->
    match_field_specs_sql_builder([{Term1, 'eq', Term2} | Rest],
        Tokens, Values, TableNames, PHCount, LogicOp, OperatorFinder, AFFN);
%% when is [{Term, MathOperator, Term}, ...].
match_field_specs_sql_builder([{Term1, Op, Term2} | Rest],
    Tokens, Values, TableNames, PHCount, LogicOp, OperatorFinder, AFFN) ->
    %% -->
    %% un-parses second term in 'Term2'.
    {Tokens1, Values1, TableNames1, PHCount1} =
        match_field_specs_sql_builder2(Term1,
            Values, TableNames, PHCount, [], OperatorFinder, AFFN),
    %% un-parses first term in 'Term1'.
    {Tokens2, Values2, TableNames2, PHCount2} =
        match_field_specs_sql_builder2(Term2,
            Values1, TableNames1, PHCount1, [], OperatorFinder, AFFN),
    %% un-parses math operator in 'Op'.
    Operator = OperatorFinder(Op, math_op),
    %% keep looping through the rest of the match spec.
    match_field_specs_sql_builder(Rest,
        Tokens ++ LogicOp ++ Tokens1 ++ Operator ++ Tokens2,
        Values2,
        TableNames2,
        PHCount2, " " ++ OperatorFinder('and', logic_op) ++ " ", OperatorFinder, AFFN);
%% case for logic operator i.e.: 'or', 'and', ...
%% must be another term after the logic operator 'Rest' cannot be '[]'.
match_field_specs_sql_builder([LogicOp | Rest],
    Tokens, Values, TableNames, PHCount, _, OperatorFinder, AFFN) when Rest /= [] ->
    %% -->
    %% un-parses logic operator in 'LogicOp'.
    Operator = " " ++ OperatorFinder(LogicOp, logic_op) ++ " ",
    match_field_specs_sql_builder(Rest,
        Tokens, Values, TableNames, PHCount, Operator, OperatorFinder, AFFN);
%% throws an ERROR! if no valid form is provided.
match_field_specs_sql_builder(InvalidForm, _, _, _, _, _, _, AFFN) ->
    case AFFN of
        true ->
            erlang:throw({error, {invalid_match_field_spec, InvalidForm}});
        false ->
            erlang:throw({error, {invalid_match_simple_field_spec, InvalidForm}})
    end.

%% *** Special cases for Terms ***

%% @private
%% when term is {Table, Field}.
match_field_specs_sql_builder2({Table, Field},
    Values, TableNames, PHCount, _, OperatorFinder, true) ->
    %% appends using a special operator '.' Table with Field "Table.Field".
    Token = atom_to_list(Table) ++ OperatorFinder('.', special_op) ++ atom_to_list(Field),
    %% -->
    {Token, Values, [Table | TableNames], PHCount};
%% when term is just a field.
match_field_specs_sql_builder2(Field,
    Values, TableNames, PHCount, _, _OperatorFinder, _) when is_atom(Field) ->
    %% -->
    Token = atom_to_list(Field),
    {Token, Values, TableNames, PHCount};
%% when term is a value, cannot be a tuple.
match_field_specs_sql_builder2(Value,
    Values, TableNames, PHCount, _, OperatorFinder, _) when not is_tuple(Value) ->
    %% -->
    %% creates a place holder for the value using a place holder count in 'PHCount'
    %% i.e.: "$1".
    Token = OperatorFinder('$', special_op) ++ integer_to_list(PHCount),
    {Token, Values ++ [Value], TableNames, PHCount + 1};
%% throws an ERROR! if no valid form is provided.
match_field_specs_sql_builder2(InvalidForm, _, _, _, _, _, AFFN) ->
    case AFFN of
        true ->
            erlang:throw({error, {invalid_match_field_spec, InvalidForm}});
        false ->
            erlang:throw({error, {invalid_match_simple_field_spec, InvalidForm}})
    end.

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Un-parses the field names from the `FieldMap' and return a string
%% containing all field separated my comma.
%% example:
%% <pre>
%%   unparse_map_fieldnames(#{id => 1, name => "test"}, ",") ->
%%       "id,name".
%% </pre>
%% @end
%%-------------------------------------------------------------------------------------------------
-spec unparse_map_fieldnames(FieldsMap, Separator) -> FieldNames when
    FieldsMap :: map(),
    Separator :: string(),
    FieldNames :: string().
unparse_map_fieldnames(FieldsMap, Separator) ->
    unparse_table_names(maps:keys(FieldsMap), Separator).

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Un-parses the specified list of keys into a string containing all
%% keys separated my comma.
%% example:
%% <pre>
%%   unparse_table_names([id, name], ",") ->
%%       "id,name".
%% </pre>
%% @throws {error, {invalid_table_spec, InvalidForm :: any()}}
%% @end
%%-------------------------------------------------------------------------------------------------
-spec unparse_table_names(Fields, Separator) -> FieldNames when
    Fields :: [atom(), ...],
    Separator :: string(),
    FieldNames :: string().
unparse_table_names(TableNames, Separator) ->
    unparse_simple_fields(TableNames, Separator, invalid_table_spec).

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Un-parses the specified list of fields into a string containing all
%% keys separated my comma for `return_fields' option.
%% example:
%% <pre>
%%   unparse_return_simple_fields([id, name], ",") ->
%%       "id,name".
%% </pre>
%% @throws {error, invalid_return_simple_fields_spec, InvalidForm :: any()}
%% @end
%%-------------------------------------------------------------------------------------------------
-spec unparse_return_simple_fields(FieldNames, Separator) -> UnParsedFieldNames when
    FieldNames :: [atom(), ...],
    Separator :: string(),
    UnParsedFieldNames :: string().
unparse_return_simple_fields(FieldNames, Separator) ->
    unparse_simple_fields(FieldNames, Separator, invalid_return_simple_fields_spec).

%% @private
unparse_simple_fields(FieldNames, Separator, ErrorMsg) ->
    case FieldNames of
        FN when is_atom(FN) ->
            atom_to_list(FieldNames);
        [] ->
            erlang:throw({error, {ErrorMsg, []}});
        _ ->
            UnParsedFieldNames = unparse_simple_fields2(FieldNames, [], ErrorMsg),
            my_reverse_join(UnParsedFieldNames, Separator)
    end.
%% @private
unparse_simple_fields2([], UnParsedFieldNames, _) ->
    UnParsedFieldNames;
unparse_simple_fields2([FieldName | Rest], UnParsedFieldNames, ErrorMsg) when is_atom(FieldName) ->
    unparse_simple_fields2(Rest, [atom_to_list(FieldName) | UnParsedFieldNames], ErrorMsg);
%% throws an ERROR! if no valid form is provided.
unparse_simple_fields2(InvalidForm, _, ErrorMsg) ->
    erlang:throw({error, {ErrorMsg, InvalidForm}}).

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Un-parses the specified list of full fields into a string containing
%% all full fields separated my comma.
%% example:
%% <pre>
%%   unparse_return_full_fields([{users, '*'}, name,
%%       {roles, [id, level]}, {users, name, alias}],
%%           fun get_postgres_operator/2) ->
%%       %% -->
%%       {"users.'*',name,roles.id,roles.level,users.name AS alias",
%%           [users, roles]}.
%% </pre>
%% Returns `{[], []}' if called with `[]'.
%% @throws {error, invalid_return_fields_spec, InvalidForm :: any()}
%% @end
%%-------------------------------------------------------------------------------------------------
-spec unparse_return_full_fields(FullFieldsSpecs, Separator, OperatorFinder) ->
    {SqlStr, TableNames} when
    FullFieldsSpecs :: proplists:proplist(),
    Separator :: string(),
    OperatorFinder :: fun(),
    SqlStr :: string(),
    TableNames :: [atom(), ...].
unparse_return_full_fields(FullFieldsSpecs, Separator, OperatorFinder) ->
    {UnParsedFields, TableNames} =
        unparse_full_fields2(FullFieldsSpecs, [], [], OperatorFinder),
    {my_reverse_join(UnParsedFields, Separator), TableNames}.

%% @private
%% base case.
unparse_full_fields2([], [], _, _) ->
    erlang:throw({error, {invalid_return_fields_spec, []}});
unparse_full_fields2([], UnParsedKeys, TableNames, _) ->
    {UnParsedKeys, TableNames};
%% {table_name(), [field_name(), ...]}.
unparse_full_fields2([{TableName, [_FieldName | _] = FieldNames} | Rest],
    UnParsedKeys, TableNames, OperatorFinder) ->
    %% -->
    NewUnParsedKeys =
        unparse_field_names(FieldNames, [], atom_to_list(TableName), OperatorFinder),
    unparse_full_fields2(Rest,
        NewUnParsedKeys ++ UnParsedKeys, [TableName | TableNames], OperatorFinder);

%% {table_name(), field_name(), field_name_alias()}.
unparse_full_fields2([{TableName, FieldName, Alias} | Rest],
    UnParsedKeys, TableNames, OperatorFinder) when FieldName =/= '*' ->
    %% -->
    unparse_full_fields2(Rest, [
            atom_to_list(TableName) ++ OperatorFinder('.', special_op)
            ++ unparse_field_name(FieldName, OperatorFinder) ++
            " " ++ OperatorFinder('ALIAS', special_op) ++ " " ++ atom_to_list(Alias)
        | UnParsedKeys], [TableName | TableNames], OperatorFinder);
%% {table_name(), '*', field_name_alias()}. Throws an ERROR!.
unparse_full_fields2([{_TableName, _FieldName, _Alias} = InvalidForm | _], _, _, _) ->
    erlang:throw({error, {invalid_return_fields_spec, InvalidForm}});
%% {table_name(), field_name()}.
unparse_full_fields2([{TableName, FieldName} | Rest],
    UnParsedKeys, TableNames, OperatorFinder) ->
    %% -->
    unparse_full_fields2(Rest,
        [atom_to_list(TableName) ++ OperatorFinder('.', special_op)
            ++ unparse_field_name(FieldName, OperatorFinder) | UnParsedKeys],
        [TableName | TableNames], OperatorFinder);
%% [field_name(), ...].
unparse_full_fields2([FieldName | Rest],
    UnParsedKeys, TableNames, OperatorFinder) when is_atom(FieldName) ->
    %% -->
    unparse_full_fields2(Rest,
        [unparse_field_name(FieldName, OperatorFinder) | UnParsedKeys], TableNames, OperatorFinder);
%% Throws an ERROR! if no valid form is provided.
unparse_full_fields2(InvalidForm, _, _, _) ->
    erlang:throw({error, {invalid_return_fields_spec, InvalidForm}}).

%% @private
unparse_field_name('*', OperatorFinder) ->
    OperatorFinder('*', special_op);
unparse_field_name(FieldName, _OperatorFinder) ->
    atom_to_list(FieldName).

%% @private
unparse_field_names([], UnParsedFieldNames, _, _) ->
    UnParsedFieldNames;
unparse_field_names([FieldName | Rest],
    UnParsedFieldNames, BaseTableName, OperatorFinder) when is_atom(FieldName) ->
    %% -- >
    unparse_field_names(Rest, [
            BaseTableName ++ OperatorFinder('.', special_op)
            ++ unparse_field_name(FieldName, OperatorFinder)
        | UnParsedFieldNames], BaseTableName, OperatorFinder);
%% throws an ERROR! if no valid form is provided.
unparse_field_names(InvalidForm, _, _, _) ->
    erlang:throw({error, {invalid_return_fields_spec, InvalidForm}}).

%%-------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Un-parses the field values and corresponding place holders from the
%% `FieldMap' and returns a tuple in which the first element is
%% a string containing one place holder for each value separated by
%% comma, the second element of the tuple is the list of all values
%% and a third element that is `PlaceHolderCountStart +
%% PlaceHolderCount'. Useful when you want to make multiple place
%% holder builders and you don't have the reference of the last count
%% used, i.e.: ` "$1, $2" ' later you need to start at ` "$3, ..." '.
%%
%% Example:
%% ```
%%   unparse_fieldvalues(1, #{id => 1, name => "test"}, "$") ->
%%       {"$1,$2",[1,"test"],3}
%% '''
%% @end
%%-------------------------------------------------------------------------------------------------
-spec unparse_fieldvalues(PlaceHolderCountStart, FieldsMap, PlaceHolder) ->
    {ValuePlaceHolders, FieldValues, Count} when
    PlaceHolderCountStart :: pos_integer(),
    FieldsMap :: map(),
    PlaceHolder :: string(),
    ValuePlaceHolders :: string(),
    FieldValues :: list(),
    Count :: pos_integer().
unparse_fieldvalues(PlaceHolderCountStart, FieldsMap, PlaceHolder) ->
    Values = maps:values(FieldsMap),
    PHCountEnd = PlaceHolderCountStart + length(Values),
    PHL = unparse_fieldvalues2([], PlaceHolder, PlaceHolderCountStart, PHCountEnd),
    ValuePlaceHolders = my_reverse_join(PHL, ","),
    {ValuePlaceHolders, Values, PHCountEnd}.

unparse_fieldvalues2(SqlStr, _, PHCount, PHCountEnd) when PHCount == PHCountEnd ->
    SqlStr;
unparse_fieldvalues2(SqlStr, PlaceHolder, PHCount, PHCountEnd) ->
    unparse_fieldvalues2([PlaceHolder ++ integer_to_list(PHCount) | SqlStr],
        PlaceHolder, PHCount + 1, PHCountEnd).

%% @private
%% thanks to Richard A. O'Keefe <ok@cs.otago.ac.nz>
my_reverse_join([X | Xs], Sep) ->
    my_reverse_join(Xs, X, Sep);
my_reverse_join([], _) ->
    "".
%% @private
my_reverse_join([], Acc, _) ->
    Acc;
my_reverse_join([Y | Ys], Acc, Sep) ->
    my_reverse_join(Ys, Y ++ Sep ++ Acc, Sep).