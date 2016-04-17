%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software
%%% @doc
%%% Manages model creation, update, etc...
%%% @end
%%% Created : 05. Sep 2015 11:26 AM
%%%-------------------------------------------------------------------
-module(model_manager).
-author("Ivan Carmenates Garcia").

-compile([export_all]).

%% API Exports
-export([
    new_model/2,
    new_model/3,
    update_model_by_id/3,
    update_model_by_id/4,
    update_model/3,
    update_model/4,
    get_model_by_id/2,
    get_model_by_id/3,
    get_model/2,
    get_model/3,
    select_models/2,
    select_models/3,
    delete_model_by_id/2,
    delete_model_by_id/3,
    delete_model/2,
    delete_model/3,
    store_model/1,
    store_model/2,
    query_model/4,
    store_block/1,
    store_block/2,
    store_block_transaction/1,
    store_block_transaction/2]).

%%-------------------------------------------------------------------
%% TYPE definitions.
%%-------------------------------------------------------------------
-type validation_error_msg() :: term() | string().
-type validation_errors() :: [validation_error_msg(), ...].
-type simple_validation_test() :: {fun(), validation_error_msg()}.
-type full_validation_tests() :: [{ValidationTag :: atom(), [simple_validation_test()]}, ...].

-opaque model_info() :: {
    ModelName :: atom(),
    TableName :: atom(),
    ModelDataMap :: map(),
    id | {id, any()} | database_manager:match_field_specs(),
    new | update}.

-export_type([validation_errors/0, simple_validation_test/0, full_validation_tests/0,
    model_info/0]).

%%%-------------------------------------------------------------------
%%% MACRO definitions.
%%%-------------------------------------------------------------------
-define(DBSessionKeyDict, ce_model_manager_dbsession).

%% -------------------------------------------------------------------
%% @doc
%% Creates a new model using a map as data-in and tries to validate
%% it using `always' default validation tag.
%%
%% Returns:
%% ```
%%   {ok, ModelInfo}
%% '''
%% `ModelInfo' is used to store the model into the database using
%% `store_model/1/2' function.
%%
%% Note:<ul>
%% <li>
%%  Function `after_validate/1' will be executed in the model if it
%%  is implemented and all validation tests pass successfully. It
%%  must return `ok' or `{error, Error}'. If it returns
%%  `{error, Error}' that will be the return value of `new_model/2/3'
%%  function call.</li></ul>
%% Example:<br/><br/>
%%   In `users_model' module, function `validation_tests/1':
%% <pre>
%%   validation_tests(ModelDataMap) ->
%%       {username := Username} = ModelDataMap,
%%       [
%%           {always, [
%%               %% username is required
%%               {fun() ->
%%                   size(Username) =/= 0
%%               end, "username cannot be empty"}
%%           ]}
%%       ].
%% </pre>
%%   Then in the controller or in another module you can call:
%% <pre>
%%   ModelDataMap = #{username => "John", age => 31},
%%   {ok, ModelInfo} = model_manager:new_model(users, ModelDataMap).
%% </pre>
%% Equivalent to:
%% ```
%%   new_model(ModelName, ModelDataMap, []).
%% '''
%% @see new_mode/3.
%% @see store_model/1.
%% @see store_model/2.
%% @end
%% -------------------------------------------------------------------
-spec new_model(ModelName, ModelDataMap) ->
    {ok, model_info()}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {validation_errors, ErrorMessages}}
    | {error, Other} when
    ModelName :: atom(),
    ModelDataMap :: map(),
    ModelModule :: atom(),
    ErrorMessages :: validation_errors(),
    Other :: term().
new_model(ModelName, ModelDataMap) ->
    new_model(ModelName, ModelDataMap, []).

%% -------------------------------------------------------------------
%% @doc
%% Creates a new model using a map as data-in and tries to validate
%% it using more than one validation tag defined in
%% `validation_tests/1'.
%%
%% Validation tags are the way of doing validation by context, you can
%% use a set of validation functions for creating a new model and
%% another set of validation functions for updating it, you can also
%% define a common sort of validation tests for both scenarios.
%%
%% Returns:
%% ```
%%   {ok, ModelInfo}
%% '''
%% `ModelInfo' is used to store the model into the database using
%% `store_model/1/2' function.
%%
%% Note:<ul>
%% <li>
%%  Function `after_validate/1' will be executed in the model if it
%%  is implemented and all validation tests pass successfully. It
%%  must return `ok' or `{error, Error}'. If it returns
%%  `{error, Error}' that will be the return value of `new_model/2/3'
%%  function call.</li></ul>
%% Example:<br/><br/>
%%   In `users_model' module function `validation_tests/1':
%% <pre>
%%   validation_tests(ModelDataMap) ->
%%       {username := Username, online := Online,
%%        password := Password} = ModelDataMap,
%%       [
%%           {common, [
%%               %% username is required
%%               {fun() ->
%%                   size(Username) =/= 0
%%               end, "username cannot be empty"}
%%           ]},
%%           {new, [
%%               %% password cannot be empty
%%               {fun() ->
%%                   size(Password) =/= 0
%%               end, "password cannot be empty"}
%%           ]},
%%           {update, [
%%               %% online must be true or false
%%               {fun() ->
%%                   (Online == true) or (Online == false)
%%               end, "online is not true or false"}
%%           ]}
%%       ].
%% </pre>
%%   Then in the controller or in another module you can call:
%% <pre>
%%   ModelDataMap = #{username => "John", password => "1eAfG6"},
%%   {ok, ModelInfo} =
%%       model_manager:new_model(users, ModelDataMap, [common, new]).
%%
%%   NewModelDataMap = #{username => "John C", online => true},
%%   {ok, NewModelInfo} =
%%       model_manager:update_model_by_id(users, Id,
%%           NewModelDataMap, [common, update]).
%% </pre>
%% Note:<ul>
%% <li>
%%  `always' validation tag will be always used if exist.</li></ul>
%% @see store_model/1.
%% @see store_model/2.
%% @end
%% -------------------------------------------------------------------
-spec new_model(ModelName, ModelDataMap, ValidationTags) ->
    {ok, model_info()}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {validation_errors, ErrorMessages}}
    | {error, Other} when
    ModelName :: atom(),
    ModelDataMap :: map(),
    ValidationTags :: [atom(), ...],
    ModelModule :: atom(),
    ErrorMessages :: validation_errors(),
    Other :: term().
new_model(ModelName, ModelDataMap, ValidationTags) ->
    %% does model validation stuffs.
    internal_query_model(ModelName, ModelDataMap, ValidationTags,
        fun(_, ModelModule, _) ->
            {ok, {ModelModule, ModelName, ModelDataMap, id, new}}
        end).

%% -------------------------------------------------------------------
%% @doc
%% Updates a model using an `Id' and a map as data-in to update and
%% tries to validate it using `always' default validation tag.
%%
%% Returns:
%% ```
%%   {ok, NewModelInfo}
%% '''
%% `NewModelInfo' is used to store the model into the database using
%% `store_model/1/2' function.
%%
%% Note:<ul>
%% <li>
%%  Function `after_validate/1' will be executed in the model if it
%%  is implemented and all validation tests pass successfully. It
%%  must return `ok' or `{error, Error}'. If it returns
%%  `{error, Error}' that will be the return value of
%%  `update_model_by_id/3/4' function call.</li></ul>
%% Example:<br/><br/>
%%   In `users_model' module, function `validation_tests/1':
%% <pre>
%%   validation_tests(ModelDataMap) ->
%%       #{age := Age} = ModelDataMap,
%%       [
%%           {always, [
%%               {fun() ->
%%                   Age > 0
%%               end, "age is invalid"}
%%           ]}
%%       ].
%% </pre>
%%   Then in the controller or in another module you can call:
%% <pre>
%%   NewModelDataMap = #{age => 32},
%%   {ok, NewModelInfo} =
%%       model_manager:update_model_by_id(users, Id, NewModelDataMap).
%% </pre>
%% Equivalent to:
%% ```
%%   update_model_by_id(ModelName, Id, NewModelDataMap, []).
%% '''
%% @see update_model_by_id/4.
%% @see store_model/1.
%% @see store_model/2.
%% @end
%% -------------------------------------------------------------------
-spec update_model_by_id(ModelName, Id, ModelDataMap) ->
    {ok, NewModelInfo}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {validation_errors, ErrorMessages}}
    | {error, Other} when
    ModelName :: atom(),
    Id :: any(),
    ModelDataMap :: map(),
    NewModelInfo :: model_info(),
    ModelModule :: atom(),
    ErrorMessages :: validation_errors(),
    Other :: term().
update_model_by_id(ModelName, Id, ModelDataMap) ->
    update_model_by_id(ModelName, Id, ModelDataMap, []).

%% -------------------------------------------------------------------
%% @doc
%% Updates a model using an `Id' and a map as data-in to update and
%% tries to validate it using more than one validation tag defined
%% in `validation_tests/1'.
%%
%% Validation tags are the way of doing validation by context, you can
%% use a set of validation functions for creating a new model and
%% another set of validation functions for updating it, you can also
%% define a common sort of validation tests for both scenarios.
%%
%% Returns:
%% ```
%%   {ok, NewModelInfo}.
%% '''
%% `NewModelInfo' is used to store the model into the database using
%% `store_model/1/2' function.
%%
%% Note:<ul>
%% <li>
%%  Function `after_validate/1' will be executed in the model if it
%%  is implemented and all validation tests pass successfully. It
%%  must return `ok' or `{error, Error}'. If it returns
%%  `{error, Error}' that will be the return value of
%%  `update_model_by_id/3/4' function call.</li></ul>
%% Example:<br/><br/>
%%   In `users_model' module function `validation_tests/1':
%% <pre>
%%   validation_tests(ModelDataMap) ->
%%       {username := Username, online := Online,
%%        password := Password} = ModelDataMap,
%%       [
%%           {common, [
%%               %% username is required
%%               {fun() ->
%%                   size(Username) =/= 0
%%               end, "username cannot be empty"},
%%           ]},
%%           {new, [
%%               %% password cannot be empty
%%               {fun() ->
%%                   size(Password) =/= 0
%%               end, "password cannot be empty"},
%%           ]},
%%           {update, [
%%               %% online must be true or false
%%               {fun() ->
%%                   (Online == true) or (Online == false)
%%               end, "online is not true or false"},
%%           ]}
%%       ].
%% </pre>
%%   Then in the controller or in another module you can call:
%% <pre>
%%   NewModelDataMap = #{age => 32},
%%   {ok, NewModelInfo} =
%%       model_manager:update_model_by_id(users, Id,
%%           NewModelDataMap, [common, update]).
%%
%%   ModelDataMap = #{username => "John", password => "1eAfG6"},
%%   {ok, ModelInfo2} =
%%       model_manager:new_model(users, ModelDataMap, [common, new]).
%% </pre>
%% Note:<ul>
%% <li>
%%  `always' validation tag will be always used if exist.</li></ul>
%% @see store_model/1.
%% @see store_model/2.
%% @end
%% -------------------------------------------------------------------
-spec update_model_by_id(ModelName, Id, ModelDataMap, ValidationTags) ->
    {ok, model_info()}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {validation_errors, ErrorMessages}}
    | {error, Other} when
    ModelName :: atom(),
    Id :: any(),
    ModelDataMap :: map(),
    ValidationTags :: [atom(), ...],
    ModelModule :: atom(),
    ErrorMessages :: validation_errors(),
    Other :: term().
update_model_by_id(ModelName, Id, ModelDataMap, ValidationTags) ->
    %% does model validation stuffs.
    internal_query_model(ModelName, ModelDataMap, ValidationTags,
        fun(_, ModelModule, _) ->
            %% creates a model info for update.
            %% gets id field name from model.
            IdFieldName = get_model_id_field_name(ModelModule),
            %% creates match fields specs for id.
            MatchFieldSpecs = [{IdFieldName, '==', Id}],
            {ok, {ModelModule, ModelName, ModelDataMap,
                {match_field_specs, MatchFieldSpecs}, update}}
        end).

%% -------------------------------------------------------------------
%% @doc
%% Updates a model using a `MatchFieldSpecs' and a map as data-in to
%% update, and tries to validate it using `always' default validation
%% tag.
%%
%% Returns:
%% ```
%%   {ok, NewModelInfo}
%% '''
%% `NewModelInfo' is used to store the model into the database using
%% `store_model/1/2' function.
%%
%% Note:<ul>
%% <li>
%%  Function `after_validate/1' will be executed in the model if it
%%  is implemented and all validation tests pass successfully. It
%%  must return `ok' or `{error, Error}'. If it returns
%%  `{error, Error}' that will be the return value of
%%  `update_model/3/4' function call.</li></ul>
%% Example:<br/><br/>
%%   In `users_model' module, function `validation_tests/1':
%% <pre>
%%   validation_tests(ModelDataMap) ->
%%       {username := Username} = ModelDataMap,
%%       [
%%           {always, [
%%               {fun() ->
%%                   Age > 0
%%               end, "Age is invalid"}
%%           ]}
%%       ].
%% </pre>
%%   Then in the controller or in another module you can call:
%% <pre>
%%   NewModelDataMap = #{age => 32},
%%   {ok, NewModelInfo} =
%%       model_manager:update_model(users,
%%           NewModelDataMap, [{id, '==', Id}]).
%% </pre>
%% Equivalent to:
%% ```
%%   update_model(ModelName, NewModelDataMap, MatchFieldSpecs, []).
%% '''
%% Note:<ul>
%% <li>
%%  See `database_manager:update/4' to learn more about
%%  `MatchFieldSpecs'.</li></ul>
%% @see update_model/4.
%% @see store_model/1.
%% @see store_model/2.
%% @see database_manager:update/4.
%% @end
%% -------------------------------------------------------------------
-spec update_model(ModelName, ModelDataMap, MatchFieldSpecs) ->
    {ok, model_info()}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {validation_errors, ErrorMessages}}
    | {error, Other} when
    ModelName :: atom(),
    ModelDataMap :: map(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    ModelModule :: atom(),
    ErrorMessages :: validation_errors(),
    Other :: term().
update_model(ModelName, ModelDataMap, MatchFieldSpecs) ->
    update_model(ModelName, ModelDataMap, MatchFieldSpecs, []).

%% -------------------------------------------------------------------
%% @doc
%% Updates a model using a `MatchFieldSpecs' and a map as data in to
%% update, and tries to validate it using more than one validation
%% tags defined in `validation_tests/1'.
%%
%% Validation tags are the way of doing validation by context, you can
%% use a set of validation functions for creating a new model and
%% another set of validation functions for updating it, you can also
%% define a common sort of validation tests for both scenarios.
%%
%% Returns:
%% ```
%%   {ok, ModelInfo}
%% '''
%% `ModelInfo' is used to store the model into the database using
%% `store_model/1/2' function.
%%
%% Note:<ul>
%% <li>
%%  Function `after_validate/1' will be executed in the model if it
%%  is implemented and all validation tests pass successfully. It
%%  must return `ok' or `{error, Error}'. If it returns
%%  `{error, Error}' that will be the return value of
%%  `update_model/3/4' function call.</li></ul>
%% Example:<br/><br/>
%%   In `users_model' module function `validation_tests/1':
%% <pre>
%%   validation_tests(ModelDataMap) ->
%%       {username := Username, online := Online,
%%        password := Password} = ModelDataMap,
%%       [
%%           {common, [
%%               %% username is required
%%               {fun() ->
%%                   size(Username) =/= 0
%%               end, "username cannot be empty"},
%%           ]},
%%           {new, [
%%               %% password cannot be empty
%%               {fun() ->
%%                   size(Password) =/= 0
%%               end, "password cannot be empty"},
%%           ]},
%%           {update, [
%%               %% online must be true or false
%%               {fun() ->
%%                   (Online == true) or (Online == false)
%%               end, "online is not true or false"},
%%           ]}
%%       ].
%% </pre>
%%   Then in the controller or in another module you can call:
%% <pre>
%%   NewModelDataMap = #{age => 32},
%%   {ok, NewModelInfo} =
%%       model_manager:update_model(users,
%%           NewModelDataMap, [{id, '==', Id}], [common, update]).
%% </pre>
%%   or
%% <pre>
%%   ModelDataMap = #{username => "John", password => "1eAfG6"},
%%   {ok, ModelInfo} =
%%       model_manager:new_model(users, ModelDataMap, [common, new]).
%% </pre>
%% Note:<ul>
%% <li>
%%  `always' validation tag will be always used if exist.</li>
%% <li>
%%  See `database_manager:update/4' to learn more about
%%  `MatchFieldSpecs'.</li></ul>
%% @see store_model/1.
%% @see store_model/2.
%% @see database_manager:update/4.
%% @end
%% -------------------------------------------------------------------
-spec update_model(ModelName, ModelDataMap, MatchFieldSpecs, ValidationTags) ->
    {ok, model_info()}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {validation_errors, ErrorMessages}}
    | {error, Other} when
    ModelName :: atom(),
    ModelDataMap :: map(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    ValidationTags :: [atom(), ...],
    ModelModule :: atom(),
    ErrorMessages :: validation_errors(),
    Other :: term().
update_model(ModelName, ModelDataMap, MatchFieldSpecs, ValidationTags) ->
    %% does model validation stuffs.
    internal_query_model(ModelName, ModelDataMap, ValidationTags,
        fun(_, ModelModule, _) ->
            %% creates a model info for update.
            {ok, {ModelModule, ModelName, ModelDataMap,
                {match_field_specs, MatchFieldSpecs}, update}}
        end).

%% -------------------------------------------------------------------
%% @doc
%% Gets a single occurrence of a model represented by its `ID', for
%% the `main_backend' database backend with a default time out.
%%
%% Returns:
%% ```
%%   {ok, ModelDataMap}
%% '''
%% `ModelDataMap' is a map containing all model fields and values.
%%
%% Example:
%% <pre>
%%   {ok, #{username := Username}} =
%%       model_manager:get_model_by_id(users, 1).
%% </pre>
%% Equivalent to:
%% ```
%%   get_model_by_id(ModelName, ID, []).
%% '''
%% Note:<ul>
%% <li>
%%  This function can be used inside a store block.</li></ul>
%% @see get_model_by_id/3.
%% @see store_block/1.
%% @see store_block/2.
%% @end
%% -------------------------------------------------------------------
-spec get_model_by_id(ModelName, ID) ->
    {ok, ModelDataMap}
    | {error, {model_without_model_module, ModelModule}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    ID :: any(),
    ModelDataMap :: map(),
    ModelModule :: atom(),
    Other :: term().
get_model_by_id(ModelName, ID) ->
    get_model_by_id(ModelName, ID, []).

%% -------------------------------------------------------------------
%% @doc
%% Gets a single occurrence a model represented by its `ID' using
%% `Options' to specify the database backend or connection timeout,
%% also the result format and other options.
%%
%% Returns:
%% ```
%%   {ok, ModelData}
%% '''
%% `ModelData' could be a map or a proplist or even raw depending on
%%             the `result_format' option.
%%
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%
%%   {timeout, Timeout :: timeout() | default_timeout}
%%
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {return_id}               %% Overrides 'return_fields'
%%                                option. Makes the function to return
%%                                {ok, ID} |
%%                                {ok, [ID, ...], {count, Count}}
%%   {result_format,
%%       raw | map | proplist} %% For `return_fields'.
%%                                `map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Note:<ul>
%% <li>
%%  Find out more about Options in database_manager:find/4.
%% </li></ul>
%% Example:
%% <pre>
%%   {ok, #{username := Username}} =
%%       model_manager:get_model_by_id(users, 1,
%%           [{backend, main_backend}, {timeout, infinity},
%%               {result_format, proplist}]).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li></ul>
%% @see store_block/1.
%% @see store_block/2.
%% @end
%% -------------------------------------------------------------------
-spec get_model_by_id(ModelName, ID, Options) ->
    {ok, ModelData}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    ID :: any(),
    Options :: proplists:proplist(),
    ModelData :: database_manager:result_data(),
    ModelModule :: atom(),
    BackendName :: atom(),
    Other :: term().
get_model_by_id(ModelName, ID, Options) ->
    case get_and_check_model_module(ModelName) of
        {ok, ModelModule} ->
            %% gets the model id field name. i.e.: 'user_id' or just 'id'.
            IdFieldName = get_model_id_field_name(ModelModule),
            %% creates or reuses a db session.
            internal_db_session(fun(DBSession) ->
                %% searches for the model in the database.
                database_manager:find(DBSession,
                    ModelName, [{IdFieldName, '==', ID}], [{limit, 1} | Options])
            end, Options);
        Error ->
            Error
    end.

%% -------------------------------------------------------------------
%% @doc
%% Gets a single occurrence of a model that match with
%% `MatchFieldSpecs' parameter from the `main_backend' database
%% backend using a default `timeout', at least you specified other
%% values in a store block call.
%%
%% Returns: `{ok, ModelDataMap}' which is a map with all model fields
%%                               and values.
%% Example:
%% <pre>
%%   {ok, #{username := Username}} =
%%       model_manager:get_model(users,
%%           #{username => "John", age => 31}).
%% </pre>
%% Equivalent to:
%% ```
%%   get_model(ModelName, MatchFieldSpecs, []).
%% '''
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li></ul>
%% @see get_model/3.
%% @see store_block/1.
%% @see store_block/2.
%% @see select_models/2.
%% @see select_models/3.
%% @end
%% -------------------------------------------------------------------
-spec get_model(ModelName, MatchFieldSpecs) ->
    {ok, ModelDataMap}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    ModelDataMap :: map(),
    ModelModule :: atom(),
    BackendName :: atom(),
    Other :: term().
get_model(ModelName, MatchFieldSpecs) ->
    get_model(ModelName, MatchFieldSpecs, []).

%% -------------------------------------------------------------------
%% @doc
%% Gets one or many occurrences of a model that match with
%% `MatchFieldSpecs' parameter using `Options' to specify the
%% database backend or connection timeout, also the result format and
%% other options.
%%
%% Note that `backend' and `timeout' options will be overridden by the
%% one you set in a store block call.
%%
%% Returns: `{ok, ModelData}' which could be a map or a proplist even
%%          raw depending on the `result_format' option.
%%
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%
%%   {timeout, Timeout :: timeout() | default_timeout}
%%
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {return_id}               %% Overrides 'return_fields'
%%                                option. Makes the function to return
%%                                {ok, ID} |
%%                                {ok, [ID, ...], {count, Count}}
%%   {result_format,
%%       raw | map | proplist} %% For `return_fields'.
%%                                `map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Note:<ul>
%% <li>
%%  Find out more about Options in `database_manager:find/4'.
%% </li></ul>
%% Example:
%% <pre>
%%   {ok, #{username := Username}} =
%%       model_manager:get_model(users,
%%           #{username => "John", age => 31},
%%           [{backend, main_backend}, {timeout, infinity}]).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li></ul>
%% @see store_block/1.
%% @see store_block/2.
%% @see database_manager:find/4.
%% @see select_models/2.
%% @see select_models/3.
%% @end
%% -------------------------------------------------------------------
-spec get_model(ModelName, MatchFieldSpecs, Options) ->
    {ok, ModelData | [ModelData | MoreModelsData]}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    Options :: proplists:proplist(),
    ModelData :: database_manager:result_data(),
    MoreModelsData :: [database_manager:result_data(), ...],
    ModelModule :: atom(),
    BackendName :: atom(),
    Other :: term().
get_model(ModelName, MatchFieldSpecs, Options) ->
    select_models(ModelName, MatchFieldSpecs, [{limit, 1} | Options]).

%% -------------------------------------------------------------------
%% @doc
%% Retrieves one or more occurrences of a model that match with
%% `MatchFieldSpecs' parameter from the `main_backend' database
%% backend using a default `timeout', at least you specified other
%% values in a store block call.
%%
%% Returns: `{ok, ModelDataMap | [ModelDataMap, ...]}' which is a map
%%          with all model fields and values.
%%
%% Example:
%% <pre>
%%   {ok, [#{username := Username} | MoreModels]} =
%%       model_manager:get_model(users,
%%           #{username => "John", age => 31}).
%% </pre>
%% Equivalent to:
%% ```
%%   get_model(ModelName, MatchFieldSpecs, []).
%% '''
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li></ul>
%% @see get_model/3.
%% @see store_block/1.
%% @see store_block/2.
%% @end
%% -------------------------------------------------------------------
-spec select_models(ModelName, MatchFieldSpecs) ->
    {ok, ModelDataMap | [ModelDataMap | MoreModels]}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    ModelDataMap :: map(),
    MoreModels :: [map(), ...],
    ModelModule :: atom(),
    BackendName :: atom(),
    Other :: term().
select_models(ModelName, MatchFieldSpecs) ->
    select_models(ModelName, MatchFieldSpecs, []).

%% -------------------------------------------------------------------
%% @doc
%% Retrieves one or many occurrences of a model that match with
%% `MatchFieldSpecs' parameter using `Options' to specify the
%% database backend or connection timeout, also the result format and
%% other options.
%%
%% Note that `backend' and `timeout' options will be overridden by the
%% one you set in a store block call.
%%
%% Returns: `{ok, ModelData | [ModelData, ...]}' which could be a map
%%           or a proplist even raw depending on the `result_format'
%%           option.
%%
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%
%%   {timeout, Timeout :: timeout() | default_timeout}
%%
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {return_id}               %% Overrides 'return_fields'
%%                                option. Makes the function to return
%%                                {ok, ID} |
%%                                {ok, [ID, ...], {count, Count}}
%%   {result_format,
%%       raw | map | proplist} %% For `return_fields'.
%%                                `map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Note:<ul>
%% <li>
%%  Find out more about Options in `database_manager:find/4'.
%% </li></ul>
%% Example:
%% <pre>
%%   {ok, [#{username := Username} | MoreModels]} =
%%       model_manager:get_model(users,
%%           #{username => "John", age => 31},
%%           [{backend, main_backend}, {timeout, infinity}]).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li></ul>
%% @see store_block/1.
%% @see store_block/2.
%% @see database_manager:find/4.
%% @end
%% -------------------------------------------------------------------
-spec select_models(ModelName, MatchFieldSpecs, Options) ->
    {ok, ModelData | [ModelData | MoreModelsData]}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    Options :: proplists:proplist(),
    ModelData :: database_manager:result_data(),
    MoreModelsData :: [database_manager:result_data(), ...],
    ModelModule :: atom(),
    BackendName :: atom(),
    Other :: term().
select_models(ModelName, MatchFieldSpecs, Options) ->
    case get_and_check_model_module(ModelName) of
        {ok, _ModelModule} ->
            %% creates or reuses a db session.
            internal_db_session(fun(DBSession) ->
                %% searches for the model in the database.
                database_manager:find(DBSession,
                    ModelName, MatchFieldSpecs, Options)
            end, Options);
        Error ->
            Error
    end.

%% -------------------------------------------------------------------
%% @doc
%% Deletes a model represented by its `ID' from the `main_backend'
%% database backend.
%%
%% Example:
%% <pre>
%%   ok = model_manager:delete_model_by_id(users, 1).
%% </pre>
%% Equivalent to:
%% ```
%%   delete_model_by_id(ModelName, ID, []).
%% '''
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li>
%% <li>
%%  This function may do delete more than one occurrence of a model
%%  if your id is not unique in the database.</li></ul>
%% @see store_block/1.
%% @see store_block/2.
%% @end
%% -------------------------------------------------------------------
-spec delete_model_by_id(ModelName, ID) ->
    ok |
    {ok, {count, Count}}
    | {error, {model_without_model_module, ModelModule}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    ID :: any(),
    Count :: pos_integer(),
    ModelModule :: atom(),
    Other :: term().
delete_model_by_id(ModelName, ID) ->
    delete_model_by_id(ModelName, ID, []).

%% -------------------------------------------------------------------
%% @doc
%% Deletes a model represented by its `ID', using `Options' to specify
%% the database backend, connection timeout and other options.
%%
%% Returns:
%% ```
%%   ok |
%%   {ok, DeletedModelData | ID} |
%%   {ok, [DeletedModelData, ...] | [ID, ...], {count, Count}}
%% '''
%% `DeletedModelData' could be a map or a proplist or even raw
%% depending on the `result_format' option.
%%
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%
%%   {timeout, Timeout :: timeout() | default_timeout}
%%
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {return_id}               %% Overrides 'return_fields'
%%                                option. Makes the function to return
%%                                {ok, ID} |
%%                                {ok, [ID, ...], {count, Count}}
%%   {result_format,
%%       raw | map | proplist} %% For 'return_fields'.
%%                                'map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Example:
%% <pre>
%%   {ok, DeletedData} =
%%       model_manager:delete_model_by_id(users, 1,
%%            [{backend, main_backend}, {timeout, infinity},
%%             {return_fields, ['*']}, {result_format, proplist}]).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li>
%% <li>
%%  This function may do delete more than one occurrence of a model
%%  if your id is not unique in the database.</li></ul>
%% @see store_block/1.
%% @see store_block/2.
%% @end
%% -------------------------------------------------------------------
-spec delete_model_by_id(ModelName, ID, Options) ->
    ok |
    {ok, DeletedModelData | ID} |
    {ok, {count, Count}} |
    {ok, [DeletedModelData, ...] | [ID, ...], {count, Count}}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    ID :: any(),
    Options :: proplists:proplist(),
    DeletedModelData :: database_manager:result_data(),
    ModelModule :: atom(),
    BackendName :: atom(),
    Other :: term().
delete_model_by_id(ModelName, ID, Options) ->
    case get_and_check_model_module(ModelName) of
        {ok, ModelModule} ->
            %% gets the model id field name. i.e.: 'user_id' or just 'id'.
            IdFieldName = get_model_id_field_name(ModelModule),
            internal_delete(ModelName, [{IdFieldName, '==', ID}], false, Options);
        Error ->
            Error
    end.

%% -------------------------------------------------------------------
%% @doc
%% Deletes one or many models that match with `MatchFieldSpecs' from
%% the `main_backend' database backend.
%%
%% Returns:
%% ```
%%   ok |
%%   {ok, {count, Count}}
%% '''
%% `Count' represents the number of occurrences that were deleted.
%%
%% Example:
%% <pre>
%%   {ok, {count, Count}} =
%%       model_manager:delete_model(users, [{age => 20}]).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li></ul>
%% @see delete_model/3.
%% @see store_block/1.
%% @see store_block/2.
%% @see database_manager:delete/4.
%% @end
%% -------------------------------------------------------------------
-spec delete_model(ModelName, MatchFieldSpecs) ->
    ok |
    {ok, {count, Count}}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    Count :: pos_integer(),
    ModelModule :: atom(),
    BackendName :: atom(),
    Other :: term().
delete_model(ModelName, MatchFieldSpecs) ->
    delete_model(ModelName, MatchFieldSpecs, []).

%% -------------------------------------------------------------------
%% @doc
%% Deletes one or many models that match with `MatchFieldSpecs' using
%% `Options' to specify the database backend, connection timeout and
%% other options.
%%
%% Returns:
%% ```
%%   ok |
%%   {ok, DeletedModelData | ID} |
%%   {ok, [DeletedModelData, ...] | [ID, ...], {count, Count}}
%% '''
%% `DeletedModelData' could be a map or a proplist or even raw
%% depending on the `result_format' option.<br/>
%% `Count' represents the number of occurrences that were deleted.
%%
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%
%%   {timeout, Timeout :: timeout() | default_timeout}
%%
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {return_id}               %% Overrides 'return_fields'
%%                                option. Makes the function to return
%%                                {ok, ID} |
%%                                {ok, [ID, ...], {count, Count}}
%%   {result_format,
%%       raw | map | proplist} %% For 'return_fields'.
%%                                'map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Example:
%% <pre>
%%   {ok, DeletedDataList, {count, Count}} =
%%       model_manager:delete_model(users, #{age => 20},
%%            [{backend, main_backend}, {timeout, infinity},
%%             {return_fields, ['*']}, {result_format, proplist}]).
%% </pre>
%% Note:<ul>
%% <li>
%%  This function can be used inside of a store block call.</li></ul>
%% @see store_block/1.
%% @see store_block/2.
%% @see database_manager:delete/4.
%% @end
%% -------------------------------------------------------------------
-spec delete_model(ModelName, MatchFieldSpecs, Options) ->
    ok |
    {ok, DeletedModelData | ID} |
    {ok, {count, Count}} |
    {ok, [DeletedModelData, ...] | [ID, ...], {count, Count}}
    | {error, {model_without_model_module, ModelModule}}
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    ModelName :: atom(),
    ID :: any(),
    MatchFieldSpecs :: database_manager:match_field_specs(),
    Options :: proplists:proplist(),
    DeletedModelData :: database_manager:result_data(),
    ModelModule :: atom(),
    BackendName :: atom(),
    Other :: term().
delete_model(ModelName, MatchFieldSpecs, Options) ->
    case get_and_check_model_module(ModelName) of
        {ok, ModelModule} ->
            ReturnId =
                case lists:member(return_id, Options) of
                    true ->
                        %% in case 'Options' have 'return_id'.
                        %% gets the id field name from the model.
                        {id_field_name, get_model_id_field_name(ModelModule)};
                    false ->
                        false
                end,
            internal_delete(ModelName, MatchFieldSpecs, ReturnId, Options);
        Error ->
            Error
    end.

%% -------------------------------------------------------------------
%% @doc
%% Stores a model using `ModelInfo' into the main_backend database.
%%
%% `ModelInfo' is returned by `new_model/2/3', `update_model/3/4' and
%% `update_model_by_id/3/4' functions.
%%
%% Example:
%% <pre>
%%   ok = model_manager:store_model(ModelInfo).
%% </pre>
%% Performance Note:<ul>
%% <li>
%%  If you have to use multiple `store_model/1/2' consecutively you
%%  should use a store block to hold the same connection for all,
%%  i.e.:
%%  ```
%%  model_manager:store_block(fun(_DBSession) ->
%%      {ok, ID} = model_manager:store_model(ModelInfo, [return_id]),
%%      {ok, ModelInfo2} =
%%          model_manager:new_model(user_blogs, #{
%%              {user_id => ID, blog_name => "test"}),
%%      ok = model_manager:store_model(ModelInfo2)
%%  end).
%%  '''</li></ul>
%% Equivalent to:
%% ```
%%   store_model(ModelInfo, []).
%% '''
%% @see store_model/2.
%% @see store_block/1.
%% @see store_block/2.
%% @end
%% -------------------------------------------------------------------
-spec store_model(ModelInfo) ->
    ok |
    {ok, {count, Count}}
    | {error, Reason} when
    ModelInfo :: model_info(),
    Count :: pos_integer(),
    Reason :: term().
store_model(ModelInfo) ->
    store_model(ModelInfo, []).

%% -------------------------------------------------------------------
%% @doc
%% Stores a model using `ModelInfo' into the database backend
%% specified in `Options'.
%%
%% `ModelInfo' is returned by `new_model/2/3', `update_model/3/4' and
%% `update_model_by_id/3/4' functions.
%%
%% Note:<ul>
%% <li>
%%  The return value depends on `Options', if it contains `return_id'
%%  then `{ok, ID}' will be returned. Otherwise if it contains
%%  `{return_fields, FieldNames}' it will return `{ok, FieldData}'.
%%  `FieldData' will be in map format if no `result_format' option
%%  is specified. Warning: if `return_id' is used `return_fields' will
%%  be ignored.</li>
%% <li>
%%  You can specify in `Options' the database backend to use and
%%  the connection timeout. If no backend is specified then
%%  `main_backend' will be used.</li></ul>
%%
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%
%%   {timeout, Timeout :: timeout() | default_timeout}
%%
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {return_id}               %% Overrides 'return_fields'
%%                                option. Makes the function to return
%%                                {ok, ID} |
%%                                {ok, [ID, ...], {count, Count}}
%%   {result_format,
%%       raw | map | proplist} %% For 'return_fields'.
%%                                'map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Example:
%% <pre>
%%   {ok, ID} =
%%       model_manager:store_model(ModelInfo, [return_id,
%%           {backend, main_backend}, {timeout, infinity}]).
%%
%%   ok = model_manager:store_model(ModelInfo,
%%            [{backend, main_backend}, {timeout, infinity}]).
%%
%%   {ok, #{id := Id, name := Name} =
%%       model_manager:store_model(ModelInfo,
%%            [{return_fields, [id, name]}, {result_format, map}]).
%% </pre>
%% You can return directly to a Django template using
%% `{result_format, proplist}', i.e.:
%% <pre>
%%   {ok, FieldData} =
%%       model_manager:store_model(ModelInfo,
%%            [{return_fields, [id, name]},
%%             {result_format, proplist}]),
%%   {ok, Body} = toppage_dtl:render([{model_data, FieldData}]),
%% </pre>
%% Later in the template you can refer to `model_data' as
%% `model_data.id' and `model_data.name'.
%%
%% Performance Note:<ul>
%% <li>
%%  If you have to use multiple `store_model/1/2' consecutively you
%%  should use a store block to hold the same connection for all,
%%  i.e.:
%%  ```
%%  model_manager:store_block(fun(_DBSession) ->
%%      {ok, ID} = model_manager:store_model(ModelInfo, [return_id]),
%%      {ok, ModelInfo2} =
%%          model_manager:new_model(user_blogs, #{
%%              {user_id => ID, blog_name => "test"}),
%%      ok = model_manager:store_model(ModelInfo2)
%%  end, [{backend, main_backend}, {timeout, infinity}]).
%%  '''</li></ul>
%% WARNING:<ul>
%% <li>
%%  Inside of a store block the backend option and timeout of a
%%  `store_model/2' call will be overridden by the one set by the
%%  `store_block/1/2' function.</li></ul>
%% @see store_block/1.
%% @see store_block/2.
%% @see database_manager:update/4.
%% @end
%% -------------------------------------------------------------------
-spec store_model(ModelInfo, Options) ->
    ok |
    {ok, FieldData| ID} |
    {ok, {count, Count}} |
    {ok, FieldData| ID, {count, Count}} |
    {ok, NoActionMessage}
    | {error, {invalid_backend, BackendName}}
    | {error, Other} when
    ModelInfo :: model_info(),
    Options :: proplists:proplist(),
    ID :: any(),
    FieldData :: database_manager:result_data(),
    Count :: pos_integer(),
    NoActionMessage :: atom(),
    BackendName :: atom(),
    Other :: term().
store_model(ModelInfo, Options) ->
    %% checks if the 'store_model' function is called inside a store block.
    internal_db_session(fun(DBSession) ->
        internal_store_model(DBSession, ModelInfo, Options)
    end, Options).

%% -------------------------------------------------------------------
%% @doc
%% Does a custom operation with a model and tries to validate it.
%% You can use more than one validation tag defined in
%% `validation_tests/1'.
%%
%% Validation tags are the way of doing validation by context, you can
%% use a set of validation functions for creating a new model and
%% another set of validation functions for updating it, you can also
%% define a common sort of validation tests for both scenarios.
%%
%% Note:<ul>
%% <li>
%%  Function `after_validate/1' will be executed in the model if it
%%  is implemented and all validation tests pass successfully. It
%%  must return `ok' or `{error, Error}'. If it returns
%%  `{error, Error}' that will be the return value of
%%  `update_model/3/4' function call.</li>
%% <li>
%%  Function in `Fun' must return either of `{ok, any()}' or
%%  `{error, term()}'.</li></ul>
%% Example:
%% <pre>
%%   model_manager:query_model(users, ModelDataMap, [common, other],
%%       fun(ModelName, ModelModule, ModelData) ->
%%           {ok, Result} = ...
%%       end).
%% </pre>
%% NOTE: `always' validation tag will be always used if exist.
%% @end
%% -------------------------------------------------------------------
-spec query_model(ModelName, ModelDataMap, ValidationTags, Fun) ->
    FunResult
    | {error, {model_without_model_module, ModelModule}}
    | {error, {validation_errors, ErrorMessages}}
    | {error, Other} when
    ModelName :: atom(),
    ModelDataMap :: map(),
    ValidationTags :: [atom(), ...],
    Fun :: fun(),
    FunResult :: {ok, any()} | {error, term()},
    ModelModule :: atom(),
    ErrorMessages :: validation_errors(),
    Other :: term().
query_model(ModelName, ModelDataMap, ValidationTags, Fun) ->
    internal_query_model(ModelName, ModelDataMap, ValidationTags, Fun).

%% -------------------------------------------------------------------
%% @doc
%% Creates a store block for the database `main_backend' with a
%% default timeout to improve performance when using multiple
%% consecutive `store_model' and `database_manager' operations.
%%
%% Note:<ul>
%% <li>
%%  To use another database backend or timeout use
%%  `store_block/2'.</li>
%% <li>
%%  `store_block/1/2' does not execute in transaction mode,
%%  for that propose use `store_block_transaction/1/2'.</li></ul>
%% Equivalent to:
%% ```
%%   store_block(Fun, []).
%% '''
%% Example:
%% <pre>
%%   model_manager:store_block(fun(_DBSession) ->
%%       {ok, ID} = model_manager:store_model(ModelInfo, [return_id]),
%%       {ok, ModelInfo2} = model_manager:new_model(user_blogs, #{
%%                              {user_id => ID, blog_name => "test"}),
%%       ok = model_manager:store_model(ModelInfo2)
%%   end).
%% </pre>
%%
%% Note:<ul>
%% <li>
%%  You can mix between high level `model_manager' functions and
%%  low level `database_manager' functions inside a store block
%%  using `DBSession' variable passed to the store block fun
%% </li></ul>.
%%
%% Example2:
%% <pre>
%%   model_manager:store_block(fun(DBSession) ->
%%       {ok, ID} = model_manager:store_model(ModelInfo,
%%                      [return_id]),
%%       database_manager:insert(DBSession,
%%           {user_messages, #{user_id => ID, message => "new user"})
%%   end).
%% </pre>
%%
%% WARNING:<ul>
%% <li>
%%  Inside of a store block the backend option and timeout of a
%%  `store_model' call will be overridden by the one specified in
%%  the `store_block/1/2' function call.</li>
%% <li>
%%  Do not use different processes inside a store block.</li></ul>
%%
%% Note:<ul>
%% <li>
%%   Using nested store blocks has no effect since only the
%%   first one creates the session and the others reuses it.</li></ul>
%% @see store_block/2.
%% @see database_manager:connection_block/1.
%% @see database_manager:connection_block/2.
%% @end
%% -------------------------------------------------------------------
-spec store_block(Fun) ->
    any()
    | {error, no_available_connection}
    | {error, Other} when
    Fun :: fun(),
    Other :: term().
store_block(Fun) ->
    internal_db_session(Fun, []).

%% -------------------------------------------------------------------
%% @doc
%% Creates a store block to improve performance when using multiple
%% consecutive `store_model' and `database_manager' operations, also
%% using `Options' to specify the database backend to use and
%% the connection timeout.
%%
%% Note:<ul>
%% <li>
%%  `store_block/1/2' does not executes in transaction mode,
%%  for that propose use `store_block_transaction/1/2'.</li></ul>
%%
%% ==== Options ====
%% ```
%%   {backend, BackendName :: atom()}
%%
%%   {timeout, Timeout :: timeout() | default_timeout}
%%
%%   {return_fields,
%%       [field_name(), ...] | ['*']}
%%
%%   {return_id}               %% Overrides 'return_fields'
%%                                option. Makes the function to return
%%                                {ok, ID} |
%%                                {ok, [ID, ...], {count, Count}}
%%   {result_format,
%%       raw | map | proplist} %% For 'return_fields'.
%%                                'map' will be the default
%%                                result format if this
%%                                options is omitted.
%% '''
%% Example:
%% <pre>
%%   model_manager:store_block(fun(_DBSession) ->
%%       {ok, ID} = model_manager:store_model(ModelInfo,
%%                      [return_id]),
%%       {ok, ModelInfo2} = model_manager:new_model(user_blogs, #{
%%                              {user_id => ID, blog_name => "test"}),
%%       ok = model_manager:store_model(ModelInfo2)
%%   end, [{backend, main_backend}, {timeout, infinity}]).
%% </pre>
%%
%% Note:<ul>
%% <li>
%%  You can mix between high level `model_manager' functions and
%%  low level `database_manager' functions inside a store block
%%  using `DBSession' variable passed to the store block fun.
%% </li></ul>
%%
%% Example2:
%% <pre>
%%   model_manager:store_block(fun(DBSession) ->
%%       {ok, ID} = model_manager:store_model(ModelInfo,
%%                      [return_id]),
%%       database_manager:insert(DBSession,
%%           {user_messages, #{user_id => ID, message => "new user"})
%%   end, [{backend, main_backend}, {timeout, infinity}]).
%% </pre>
%%
%% WARNING:<ul>
%% <li>
%%  Inside of a store block the backend option and timeout of a
%%  `store_model' call will be overridden by the one specified in
%%  the `store_block/1/2' function call.</li>
%% <li>
%%  Do not use different processes inside a store block.</li></ul>
%% Note:<ul>
%% <li>
%%  Using nested store blocks has no down-side effects since only
%%  the first one creates the session and the others reuses it.
%% </li></ul>
%% @end
%% -------------------------------------------------------------------
-spec store_block(Fun, Options) ->
    any()
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    Fun :: fun(),
    Options :: proplists:proplist(),
    BackendName :: atom(),
    Other :: term().
store_block(Fun, Options) ->
    internal_db_session(Fun, Options).

%% -------------------------------------------------------------------
%% @doc
%% Creates a store block for the database `main_backend' with a
%% default connection timeout to improve performance when using
%% multiple consecutive `store_model' and `database_manager'
%% operations, also creates a database transaction context.
%%
%% Note:<ul>
%% <li>
%%  To use another database backend and timeout with transaction
%%  context use `store_block_transaction/2'.</li></ul>
%% Equivalent to:
%% ```
%%   store_block_transaction(Fun, []).
%% '''
%% Example:
%% <pre>
%%   model_manager:store_block_transaction(fun(_DBSession) ->
%%       {ok, ID} = model_manager:store_model(ModelInfo,
%%                      [return_id]),
%%       {ok, ModelInfo2} = model_manager:new_model(user_blogs, #{
%%                              {user_id => ID, blog_name => "test"}),
%%       ok = model_manager:store_model(ModelInfo2)
%%   end).
%% </pre>
%% Note:<ul>
%% <li>
%%  You can mix between high level `model_manager' functions and
%%  low level `database_manager' functions inside a store block
%%  using `DBSession' variable passed to the store block fun.
%% </li></ul>
%% Example2:
%% <pre>
%%   model_manager:store_block_transaction(fun(DBSession) ->
%%       {ok, ID} = model_manager:store_model(ModelInfo,
%%                      [return_id]),
%%       database_manager:transaction(DBSession, fun() ->
%%           database_manager:insert(DBSession, {user_messages,
%%               #{user_id => ID, message => "new user"})
%%       end)
%%   end).
%% </pre>
%% WARNING:<ul>
%% <li>
%%  Inside of a store block the backend option and timeout
%%  of a `store_model' call will be overridden by the one
%%  specified in the `store_block_transaction/1/2'
%%  function call.</li>
%% <li>
%%  Do not use different processes inside a store block.
%% </li></ul>
%% @see store_block_transaction/2.
%% @see database_manager:connection_block_transaction/1.
%% @see database_manager:connection_block_transaction/2.
%% @end
%% -------------------------------------------------------------------
-spec store_block_transaction(Fun) ->
    any()
    | {error, no_available_connection}
    | {error, Other} when
    Fun :: fun(),
    Other :: term().
store_block_transaction(Fun) ->
    store_block_transaction(Fun, []).

%% -------------------------------------------------------------------
%% @doc
%% Creates a store block for a custom database backend and timeout
%% specified in `Options' to improve performance when using multiple
%% consecutive `store_model' and `database_manager' operations, also
%% creates a database transaction context.
%%
%% Note:<ul>
%% <li>
%%  You can specify in `Options' the database backend to use and
%%  the connection timeout.</li></ul>
%% Example:
%% <pre>
%%   model_manager:store_block_transaction(fun(_DBSession) ->
%%       {ok, ID} = model_manager:store_model(ModelInfo,
%%                      [return_id]),
%%       {ok, ModelInfo2} = model_manager:new_model(user_blogs, #{
%%                              {user_id => ID, blog_name => "test"}),
%%       ok = model_manager:store_model(ModelInfo2)
%%   end, [{backend, main_backend}, {timeout, infinity}]).
%% </pre>
%% Note:<ul>
%% <li>
%%  You can mix between high level `model_manager' functions and low
%%  level `database_manager' functions inside a store block using
%%  `DBSession' variable passed to the store block fun.</li></ul>
%% Example2:
%% <pre>
%%   model_manager:store_block_transaction(fun(DBSession) ->
%%       {ok, ID} = model_manager:store_model(ModelInfo,
%%                      [return_id]),
%%       database_manager:transaction(DBSession, fun() ->
%%           database_manager:insert(DBSession, {user_messages,
%%                   #{user_id => ID, message => "new user"})
%%       end)
%%   end, [{backend, main_backend}, {timeout, infinity}]).
%% </pre>
%% WARNING:<ul>
%% <li>
%%  Inside of a store block the backend option and timeout of a
%%  `store_model' call will be overridden by the one specified in
%%  the `store_block_transaction/1/2' function call.</li>
%% <li>
%%  Do not use different processes inside a store block.</li></ul>
%% @see database_manager:connection_block_transaction/1.
%% @see database_manager:connection_block_transaction/2.
%% @end
%% -------------------------------------------------------------------
-spec store_block_transaction(Fun, Options) ->
    any()
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Other} when
    Fun :: fun(),
    Options :: proplists:proplist(),
    BackendName :: atom(),
    Other :: term().
store_block_transaction(Fun, Options) ->
    internal_db_session(fun(DBSession) ->
        %% does the transaction with the current store block.
        database_manager:transaction(DBSession, fun() -> Fun(DBSession) end)
    end, Options).

%% -------------------------------------------------------------------
%% @doc
%% Gets the current database connection session created by
%% `store_block/1/2' function.
%% @end
%% -------------------------------------------------------------------
-spec get_current_db_session() -> DBSession | undefined when
    DBSession :: database_manager:db_session().
get_current_db_session() ->
    erlang:get(?DBSessionKeyDict).

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Executes 'validation_tests/1' function in 'ModelModule' if exists.
%% @end
%% -------------------------------------------------------------------
-spec exec_validation_tests(ModelModule, ModelDataMap, ValidationTags) ->
    ok
    | {error, {validation_error, Errors}} when
    ModelModule :: atom(),
    ModelDataMap :: map(),
    ValidationTags :: [atom(), ...],
    Errors :: validation_errors().
exec_validation_tests(ModelModule, ModelDataMap, ValidationTags) ->
    %% checks if 'ModelModule' has 'validation_tests/1' function implemented.
    case erlang:function_exported(ModelModule, validation_tests, 1) of
        true ->
            %% tries to validates the model.
            case do_validation(ValidationTags, ModelModule:validation_tests(ModelDataMap)) of
                ok ->
                    ok;
                {error, Errors} ->
                    {error, {validation_error, Errors}}
            end;
        false ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Does the model data validation.
%% @end
%% -------------------------------------------------------------------
-spec do_validation(ValidationTags, FullValidationTests) ->
    ok
    | {error, ErrorMessages} when
    ValidationTags :: [atom(), ...],
    FullValidationTests :: full_validation_tests(),
    ErrorMessages :: validation_errors().
do_validation(_, []) ->
    ok;
do_validation(ValidationTags, FullValidationTests) ->
    %% gets all validation tests for specified 'ValidationTags'.
    ValidationTests = filter_validation_tests_by_tags(ValidationTags, FullValidationTests),
    %% does the assertion on each validation function.
    Messages = lists:foldr(
        fun({F, ErrorMessage}, Acc) ->
            case F() of
                true ->
                    %% if validates ok.
                    Acc;
                false ->
                    %% if validation fails, returns fail assertion message.
                    [ErrorMessage | Acc]
            end
        end, [], ValidationTests),
    %% checks if any validation was fail.
    case Messages == [] of
        true ->
            %% if all assertion test where fine.
            ok;
        false ->
            %% if some fail.
            {error, Messages}
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Converts a full validation tests list into a filtered list of
%% simple validation tests.
%% @end
%% -------------------------------------------------------------------
-spec filter_validation_tests_by_tags(ValidationTags, FullValidationTests) ->
    [simple_validation_test()] when
    ValidationTags :: atom(),
    FullValidationTests :: full_validation_tests().
filter_validation_tests_by_tags(ValidationTags, FullValidationTests) ->
    NewValidationTags = [always | ValidationTags],
    LOFT =
        lists:filtermap(fun({Tag, ValidationTests}) ->
            case lists:member(Tag, NewValidationTags) of
                true ->
                    {true, ValidationTests};
                _ ->
                    false
            end
        end, FullValidationTests),
    %% joins all filtered lists of tests into one list.
    lists:merge(LOFT).

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Executes 'after_validate/1' function in 'ModelModule' if exists.
%% @end
%% -------------------------------------------------------------------
-spec exec_after_validate(ModelModule, ModelDataMap) ->
    ok
    | {error, Error} when
    ModelModule :: atom(),
    ModelDataMap :: map(),
    Error :: term().
exec_after_validate(ModelModule, ModelDataMap) ->
    %% checks if exist 'after_validate/1' function in 'ModelModule'.
    case erlang:function_exported(ModelModule, after_validate, 1) of
        true ->
            %% calls 'after_validate/1' function in 'Model' module.
            ModelModule:after_validate(ModelDataMap);
        false ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Query model does new, update or any other custom behavior taking
%% the advantage of model 'validation_tests/1' and 'after_validate/1'
%% functions.
%% NOTE: function in 'Fun' must return either of '{ok, any()}' or
%%       '{error, term()}'.
%% @end
%% -------------------------------------------------------------------
-spec internal_query_model(ModelName, ModelDataMap, ValidationTags, Fun) ->
    FunResult
    | {error, {model_without_model_module, ModelModule}}
    | {error, {validation_errors, ErrorMessages}}
    | {error, Other} when
    ModelName :: atom(),
    ModelDataMap :: map(),
    ValidationTags :: [atom(), ...],
    Fun :: fun(),
    FunResult :: {ok, any()} | {error, term()},
    ModelModule :: atom(),
    ErrorMessages :: validation_errors(),
    Other :: term().
internal_query_model(ModelName, ModelDataMap, [], Fun) ->
    internal_query_model(ModelName, ModelDataMap, [always], Fun);
internal_query_model(ModelName, ModelDataMap, [Tag | _] = ValidationTags, Fun) when is_atom(Tag) ->
    %% checks if 'ModelModule' module exist.
    %% NOTE: the module has to be previously loaded for this to work, luckily the framework
    %%       does it for us when first load.
    case get_and_check_model_module(ModelName) of
        {ok, ModelModule} ->
            %% executes 'validation_tests/1' function in 'Model' if exists.
            case exec_validation_tests(ModelModule, ModelDataMap, ValidationTags) of
                ok ->
                    %% executes 'after_validate/1' function in 'Model' if exists.
                    case exec_after_validate(ModelModule, ModelDataMap) of
                        ok ->
                            %% if after_validate returns 'ok' then executes the
                            %% function in 'Fun'.
                            case Fun(ModelName, ModelModule, ModelDataMap) of
                                {ok, Result} ->
                                    {ok, Result};
                                {error, Error2} ->
                                    {error, Error2}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, {validation_error, Errors}} ->
                    {error, {validation_error, Errors}}
            end;
        Error ->
            Error
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Inserts or updates a model into a database.
%%
%% Returns the model id or specified fields if 'Options' contains
%% 'return_id' or '{return_fields, FieldNames}'.
%% @end
%% -------------------------------------------------------------------
-spec internal_store_model(DBSession, ModelInfo, Options) ->
    ok |
    {ok, FieldData | ID} |
    {ok, {count, Count}} |
    {ok, FieldData | [ID, ...], {count, Count}} |
    {ok, NoActionMessage}
    | {error, Reason} when
    DBSession :: database_manager:db_session(),
    ModelInfo :: model_info(),
    Options :: proplists:proplist(),
    ID :: any(),
    FieldData :: database_manager:result_data(),
    Count :: pos_integer(),
    NoActionMessage :: atom(),
    Reason :: term().
internal_store_model(DBSession, ModelInfo, Options) ->
    %% gets 'ModelInfo' data.
    {ModelModule, ModelName, ModelDataMap, IdOrMatchFieldSpec, NewOrUpdate} = ModelInfo,
    %% gets return fields.
    ReturnId =
        case lists:member(return_id, Options) of
            true ->
                %% in case 'Options' have 'return_id'.
                %% gets the id field name from the model.
                {id_field_name, get_model_id_field_name(ModelModule)};
            false ->
                false
        end,
    case NewOrUpdate of
        new ->
            internal_insert(DBSession,
                ModelName, ModelDataMap, ReturnId, Options);
        update ->
            internal_update(DBSession,
                ModelName, ModelDataMap, IdOrMatchFieldSpec, ReturnId, Options)
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Inserts a model into the database.
%%
%% @end
%% -------------------------------------------------------------------
-spec internal_insert(DBSession, ModelName, ModelDataMap, ReturnId, Options) ->
    ok |
    {ok, FieldData | ID} |
    {ok, {count, Count}} |
    {ok, FieldData | [ID, ...], {count, Count}} |
    {ok, nothing_to_insert}
    | {error, Reason} when
    DBSession :: database_manager:db_session(),
    ModelName :: atom(),
    ModelDataMap :: map(),
    ReturnId :: {id_field_name, IdFieldName :: atom()} | false,
    Options :: proplists:proplist(),
    Count :: pos_integer(),
    ID :: any(),
    FieldData :: database_manager:result_data_for_insert(),
    Reason :: term().
internal_insert(DBSession, ModelName, ModelDataMap, ReturnId, Options) ->
    NewOptions =
        case ReturnId of
            false ->
                Options;
            {id_field_name, IdFieldName} ->
                %% ignores 'ReturnFields', so uses only 'return_id'.
                [{result_format, proplist}, {return_fields, [IdFieldName]}]
        end,
    R = database_manager:insert(DBSession, {ModelName, ModelDataMap}, NewOptions),
    process_reply(R, {no_action_message, nothing_to_insert}).

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Updates a model in the database.
%% @end
%% -------------------------------------------------------------------
-spec internal_update(
    DBSession, ModelName, ModelDataMap, MatchFieldSpecs, ReturnId, ReturnFields) ->
    ok |
    {ok, FieldData | ID} |
    {ok, {count, Count}} |
    {ok, FieldData | [ID, ...], {count, Count}} |
    {ok, nothing_to_update}
    | {error, Reason} when
    DBSession :: database_manager:db_session(),
    ModelName :: atom(),
    ModelDataMap :: map(),
    MatchFieldSpecs :: {match_field_specs, database_manager:simple_return_field_specs()},
    ReturnId :: {id_field_name, IdFieldName :: atom()} | false,
    ReturnFields :: [atom(), ...],
    Count :: pos_integer(),
    ID :: any(),
    FieldData :: database_manager:result_data(),
    Reason :: term().
internal_update(DBSession, ModelName, ModelDataMap,
    {match_field_specs, MatchFieldSpecs}, ReturnId, Options) ->
    NewOptions =
        case ReturnId of
            false ->
                Options;
            {id_field_name, IdFieldName} ->
                %% ignores 'ReturnFields', so uses only 'return_id'.
                [{result_format, proplist}, {return_fields, [IdFieldName]}]
        end,
    R = database_manager:update(DBSession, {ModelName, ModelDataMap},
        MatchFieldSpecs, NewOptions),
    process_reply(R, {no_action_message, nothing_to_update}).

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Deletes a model in the database.
%% @end
%% -------------------------------------------------------------------
-spec internal_delete(ModelName, MatchFieldSpecs, ReturnId, Options) ->
    ok |
    {ok, FieldData | ID} |
    {ok, {count, Count}} |
    {ok, FieldData | [ID, ...], {count, Count}} |
    {ok, nothing_to_update}
    | {error, Reason} when
    ModelName :: atom(),
    MatchFieldSpecs :: {match_field_specs, database_manager:simple_return_field_specs()},
    ReturnId :: {id_field_name, IdFieldName :: atom()} | false,
    Options :: proplists:proplist(),
    FieldData :: database_manager:result_data(),
    Count :: pos_integer(),
    ID :: any(),
    Reason :: term().
internal_delete(ModelName, MatchFieldSpecs, ReturnId, Options) ->
    NewOptions =
        case ReturnId of
            false ->
                Options;
            {id_field_name, IdFieldName} ->
                %% ignores 'ReturnFields', so uses only 'return_id'.
                [{result_format, proplist}, {return_fields, [IdFieldName]}]
        end,
    %% creates or reuses a db session.
    internal_db_session(fun(DBSession) ->
        %% deletes the model from the database.
        R = database_manager:delete(DBSession, ModelName, MatchFieldSpecs, NewOptions),
        process_reply(R, {no_action_message, nothing_to_delete})
    end, Options).

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Processes the result value of an insert and an update.
%% @end
%% -------------------------------------------------------------------
%% ok |
%% {ok, FieldData | ID} |
%% {ok, {count, Count}} |
%% {ok, FieldData | [ID, ...], {count, Count}} |
%% {ok, NoActionMessage}
%% Other
process_reply(R, {no_action_message, NoActionMessage}) ->
    case R of
        {ok, 1} ->
            ok;
        {ok, N} when N > 0 ->
            {ok, {count, N}};
        {ok, 1, [[{_, ID}]]} ->
            {ok, ID};
        {ok, N, [[{_, _ID}] | _] = ListOfIDs} when N > 1 ->
            %% returns just the ids in a list.
            {ok, lists:map(fun([{_, ID}]) -> ID end, ListOfIDs), {count, N}};
        {ok, 1, FieldData} ->
            {ok, FieldData};
        {ok, N, FieldData} when N > 0 ->
            {ok, FieldData, {count, N}};
        {ok, 0} ->
            {ok, NoActionMessage};
        Other ->
            Other
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Creates or reuses an db session.
%% Example:
%%   internal_db_session(fun(DBSession) ->
%%     ...
%%   end, [{backend, main_backend}, {timeout, infinity}]).
%% @end
%% -------------------------------------------------------------------
-spec internal_db_session(Fun, Options) ->
    FunReply
    | {error, {invalid_backend, BackendName}}
    | {error, no_available_connection}
    | {error, Reason} when
    Fun :: fun(),
    Options :: proplists:proplist(),
    BackendName :: atom(),
    FunReply :: any(),
    Reason :: term().
internal_db_session(Fun, Options) ->
    %% checks if the 'internal_db_session' function is called inside another
    %% db session.
    case get_current_db_session() of
        undefined ->
            %% if it is not, then create a db session.
            database_manager:connection_block(fun(DBSession) ->
                %% sets the db session dict.
                erlang:put(?DBSessionKeyDict, DBSession),
                %% executes the context function.
                try
                    Fun(DBSession)
                after
                    %% sets the db session dict.
                    erlang:erase(?DBSessionKeyDict)
                end
            end, Options);
        DBSession ->
            %% if so, then use the current session.
            %% executes the context function.
            Fun(DBSession)
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the id field name from the model.
%% NOTE: this function must be called with 'Model' module already
%%       loaded.
%% @end
%% -------------------------------------------------------------------
get_model_id_field_name(Model) ->
    case erlang:function_exported(Model, id_field_name, 0) of
        true ->
            Model:id_field_name();
        false ->
            id
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Gets the model module for a model which is the model name with
%% '_model' added. i.e.: users -> users_model.
%% @end
%% -------------------------------------------------------------------
-spec get_model_module(ModelName :: atom()) -> ModelModule :: atom().
get_model_module(ModelName) ->
    %% gets the model module by adding '_model' to 'ModelName'.
    list_to_atom(atom_to_list(ModelName) ++ "_model").

%% -------------------------------------------------------------------
%% @private
%% @doc
%% gets also checks if there is a model module for a model.
%% @end
%% -------------------------------------------------------------------
-spec get_and_check_model_module(ModelName) ->
    {ok, ModelModule}
    | {error, {model_without_model_module, ModelModule}} when
    ModelName :: atom(),
    ModelModule :: atom().
get_and_check_model_module(ModelName) ->
    ModelModule = get_model_module(ModelName),
    case erlang:function_exported(ModelModule, module_info, 0) of
        true ->
            {ok, ModelModule};
        false ->
            debug_logger:log_error(
                "There is not a module named: '~p' for model: '~p'", [ModelModule, ModelName]),
            {error, {model_without_model_module, ModelModule}}
    end.