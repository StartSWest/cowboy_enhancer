%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% This is an util module for requests.
%%% @end
%%% Created : 20. Jul 2015 1:18 PM
%%%-------------------------------------------------------------------
-module(request_manager).
-author("Ivan Carmenates Garcia").

%% -------------------------------------------------------------------
%% API Exports
%% -------------------------------------------------------------------
-export([
    handle_req/3,
    handle_req/4,
    map_request_params/2]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc
%% Parses the requested url params into a map().
%% NOTE:
%%   url params should be in the following format:
%%   http://host/handler-name/action-name[/param_name-param_value[-param_value_n]/...]
%% i.e.:
%%   url: ".../id-1".
%%   #{id := ID} = map_request_params([id], Req).
%%
%%   url: ".../blog_id-1/blogpost_id-5".
%%   #{blog_id := BlogID, blogpost_id := BlogPostID} =
%%       map_request_params([blog_id, blogpost_id], Req).
%%
%% NOTE: 'undefined' will be returned for the value of the param names
%%       that doesn't exists in the request url.
%% @end
%% -------------------------------------------------------------------
-spec map_request_params(ParamNames, Req) -> map() | undefined when
    ParamNames :: [atom(), ...],
    Req :: cowboy_req:req().
map_request_params(ParamNames = [ParamName | _], Req) when is_atom(ParamName) ->
    map_params(ParamNames, Req);
map_request_params([], _) ->
    exit({badarg, "function map_request_params/2 cannot be called without at least 1 param name'"});
map_request_params(_, _) ->
    exit({badarg, "function map_request_params/2 called with invalid 'ParamNames' argument"}).

%% -------------------------------------------------------------------
%% @doc
%% Routes the client's requests to the specific actions defined
%% in 'Opts'.
%% @end
%% -------------------------------------------------------------------
-spec handle_req(Module, Req, Opts) ->
    {ok, Req2, Opts2} | {stop, Req2} | {suspend, Module2, Function, Arguments} when
    Module :: atom(),
    Req :: cowboy_req:req(),
    Opts :: any(),
    Req2 :: cowboy_req:req(),
    Opts2 :: any(),
    Module2 :: atom(),
    Function :: atom(),
    Arguments :: any().
handle_req(Module, Req, Opts) ->
    execute_action(Req, Opts, Module, 3, fun(Action, Method) ->
        Module:Action(Method, Req, Opts)
    end).

% -------------------------------------------------------------------
%% @doc
%% Routes the client's authenticated requests to the specific actions
%% defined in 'Opts'.
%% @end
%% -------------------------------------------------------------------
-spec handle_req(Module, Req, Opts, SessionID) ->
    {ok, Req2, Opts2} | {stop, Req2} | {suspend, Module2, Function, Arguments} when
    Module :: atom(),
    Req :: cowboy_req:req(),
    Opts :: any(),
    SessionID :: binary(),
    Req2 :: cowboy_req:req(),
    Opts2 :: any(),
    Module2 :: atom(),
    Function :: atom(),
    Arguments :: any().
handle_req(Module, Req, Opts, SessionID) ->
    execute_action(Req, Opts, Module, 4, fun(Action, Method) ->
        Module:Action(Method, Req, Opts, SessionID)
    end).

%% -------------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------------

%% tries to execute an action defined in 'Opts' for the module in 'Module'.
execute_action(Req, Opts, Module, ParamCount, Run) when is_function(Run) ->
    %% obtains the request method.
    Method = cowboy_req:method(Req),
    %% gets the action to execute.
    case proplists:get_value(action, Opts) of
        undefined ->
            %% if no action defined just do nothing.
            {ok, Req, Opts};
        Action ->
            %% checks that the action is implemented.
            %% NOTE: This could be used safely here because the module is already loaded
            %% since it is the one who calls this function usually in 'init/2/3'.
            case erlang:function_exported(Module, Action, ParamCount) of
                true ->
                    try
                        %% now that Action and Method is obtained function
                        %% in 'Run' should do the thing.
                        case Run(Action, Method) of
                            {ok, Req2, Opts} ->
                                {ok, Req2, Opts};
                            {stop, Req2} ->
                                {stop, Req2};
                            {suspend, Module, Function, Arguments} ->
                                {suspend, Module, Function, Arguments};
                            Other ->
                                debug_logger:log_error(
                                    erlang:get_stacktrace(),
                                    "~p:~p/~p has invalid return value:~n~p",
                                    [Module, Action, ParamCount, Other]),
                                Other
                        end
                    catch
                        error:function_clause ->
                            %% in case the action is implemented but not
                            %% for an specific method.
                            %% Method not allowed.
                            cowboy_req:reply(405, Req)
                    end;
                false ->
                    debug_logger:log_error(
                        erlang:get_stacktrace(),
                        "request_handler: '~p.erl' does not implements action:"
                        " ~p/~p", [Module, Action, ParamCount]),
                    {ok, Req, Opts}
            end
    end.

-spec map_params(ParamNames, Req) -> map() when
    ParamNames :: [atom(), ...],
    Req :: cowboy_req:req().
map_params(ParamNames, Req) ->
    RequestParams =
        case cowboy_req:path_info(Req) of
            undefined ->
                [];
            CrudeRequestParams ->
                lists:filtermap(fun(RP) ->
                    parse_request_param(RP, ParamNames)
                end, CrudeRequestParams)
        end,
    MissingParams =
        lists:filtermap(fun(P) ->
            case find_param(P, RequestParams) of
                false ->
                    %% if no param 'P' is found in 'RequestParams' then
                    %% add it to the list with 'undefined' as its value.
                    {true, {P, undefined}};
                _ ->
                    %% do not add the already added params.
                    false
            end
        end, ParamNames),
    MixedParams = lists:merge(RequestParams, MissingParams),
    maps:from_list(MixedParams).

-spec parse_request_param(RequestParam, ParamNames) ->
    {true, {RequestParamName, RequestParamValue}} | false when
    RequestParam :: binary(),
    ParamNames :: [atom(), ...],
    RequestParamName :: atom(),
    RequestParamValue :: any().
parse_request_param(<<>>, _) ->
    %% this is for the case when the url is in the format:
    %% ..//..
    false;
parse_request_param(RequestParam, ParamNames) ->
    %% converts 'RequestParam' to string.
    SRP = bitstring_to_list(RequestParam),
    %% splits the converted 'RequestParam' into request param name
    %% and value or values.
    SPRTokens = string:tokens(SRP, "-"),
    %% retrieves the request param name.
    RequestParamName = list_to_atom(erlang:hd(SPRTokens)),
    %% checks if the 'RequestParamName' is in the specified 'Params'.
    case find_request_param(RequestParamName, ParamNames) of
        true ->
            Values = erlang:tl(SPRTokens),
            RequestParamValue =
                case erlang:length(Values) of
                    0 ->
                        undefined;
                    1 ->
                        erlang:hd(Values);
                    _ ->
                        Values
                end,
            {true, {RequestParamName, RequestParamValue}};
        false ->
            false
    end.

-spec find_request_param(RequestParamName, Params) -> true | false when
    RequestParamName :: atom(),
    Params :: [ParamName, ...],
    ParamName :: atom().
find_request_param(RequestParamName, Params = [_ParamName | _]) ->
    %% finds a 'RequestParamName' in 'Params' when 'Params'
    %% is in simple list format '[ParamName|_]'.
    lists:member(RequestParamName, Params);
find_request_param(_RequestParamName, []) ->
    %% returns true if there is not 'Params'.
    true.

-spec find_param(ParamName, ParsedRequestParams) -> true | false when
    ParamName :: atom(),
    ParsedRequestParams :: [{atom(), any()}, ...].
find_param(ParamName, ParsedRequestParams) ->
    lists:keymember(ParamName, 1, ParsedRequestParams).