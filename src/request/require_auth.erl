%%-------------------------------------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2015, Ivanco Software Corporation
%% @doc
%% This is a cowboy subprotocol.
%% @end
%% Created : 17. Jul 2015 3:14 PM
%%-------------------------------------------------------------------------------------------------
-module(require_auth).
-author("Ivan Carmenates Garcia").

-behavior(cowboy_sub_protocol).

%%-------------------------------------------------------------------------------------------------
%% Callback Exports
%%-------------------------------------------------------------------------------------------------
-export([upgrade/6]).

%%-------------------------------------------------------------------------------------------------
%% Callback Functions
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Changes from a guest session to an authenticated one.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec upgrade(Req, Env, Handler, HandlerState, Timeout, Hibernate) ->
    {ok, Req, Env} | {stop, Req} when
    Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env(),
    Handler :: module(),
    HandlerState :: any(),
    Timeout :: infinity,
    Hibernate :: run.
upgrade(Req, Env, Handler, HandlerState, infinity, run) ->
    %% checks if the request_handler implements ensure_logged_in/2 function.
    %% NOTE: This function could be used safely here because the module is already loaded
    %% since the upgrade comes from the module.
    case erlang:function_exported(Handler, ensure_logged_in, 2) of
        true ->
            try
                case Handler:ensure_logged_in(Req, HandlerState) of
                    {true, SessionID} ->
                        %% if the authentication is true then execute 'init/3', if implemented,
                        %% with 'SessionID'.
                        case Handler:init(Req, HandlerState, SessionID) of
                            {ok, Req2, _} ->
                                terminate(normal, Req2, Env, HandlerState, Handler);
                            {stop, Req2} ->
                                terminate(stop, Req2, Env, HandlerState, Handler);
                            {suspend, Module, Function, Arguments} ->
                                %% TODO: check how to implement 'terminate' for suspend.
                                {suspend, Module, Function, Arguments};
                            Other ->
                                Other
                        end;
                    {ok, Req3, _} ->
                        terminate(normal, Req3, Env, HandlerState, Handler);
                    {stop, Req3} ->
                        terminate(stop, Req3, Env, HandlerState, Handler);
                    Other2 ->
                        debug_logger:log_error(
                            erlang:get_stacktrace(),
                            "~p:ensure_logged_in/2 is not returning a valid value:~n~p",
                            [Handler, Other2]),
                        Other2
                end
            catch Class:Reason ->
                terminate({crash, Class, Reason}, Req, Env, HandlerState, Handler),
                exit([
                    {class, Class},
                    {reason, Reason},
                    {stacktrace, erlang:get_stacktrace()},
                    {req, cowboy_req:to_list(Req)},
                    {state, HandlerState}
                ])
            end;
        false ->
            debug_logger:log_error(
                erlang:get_stacktrace(),
                "request_handler: '~p.erl' does not implements ensure_logged_in/2",
                [Handler]),
            {stop, Req}
    end.

terminate(Reason, Req, Env, HandlerState, Handler) ->
    Result = cowboy_handler:terminate(Reason, Req, HandlerState, Handler),
    {ok, Req, [{result, Result} | Env]}.