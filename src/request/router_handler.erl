%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% Handles routes and code recompiling and reloading when in
%%% development mode.
%%% @end
%%% Created : 07. Aug 2015 7:58 PM
%%%-------------------------------------------------------------------
-module(router_handler).
-author("Ivan Carmenates Garcia").

-behaviour(cowboy_middleware).

%% -------------------------------------------------------------------
%% Callback Exports
%% -------------------------------------------------------------------
-export([
    execute/2]).

%% -------------------------------------------------------------------
%% Callback Functions
%% -------------------------------------------------------------------

%% this will only work if in 'development_mode'.
-ifdef(dev_mode).
execute(Req, Env) ->
    %% ********************************************************
    %% NOTE: If in development mode tries to reload the recently
    %% changed files.
    case ce_control:rebuild_all() of
        ok ->
            execute2(Req, Env);
        error ->
            Req2 = cowboy_req:reply(200,
                [{<<"content-type">>, <<"text/html">>}],
                "You have some errors, check console log for more details.", Req),
            {ok, Req2, Env}
    end.
-else.
%% in production.
execute(Req, Env) ->
    execute2(Req, Env).
-endif.

execute2(Req, Env) ->
    %% retrieves the request handler name.
    case cowboy_req:binding(handler, Req) of
        undefined ->
            %% this is the case when other routes matches first.
            {ok, Req, Env};
        HandlerName ->
            %% retrieves the action name for the request handler.
            case cowboy_req:binding(action, Req) of
                undefined ->
                    %% this is the case when other routes matches first.
                    {ok, Req, Env};
                ActionName ->
                    %% prepares cowboy for execution of 'ActionName' in 'Handler'.
                    execute3(HandlerName, ActionName, Req, Env)
            end
    end.

execute3(HandlerName, ActionName, Req, Env) ->
    %% when .../handler/action/[...] route matches.
    %% renames 'HandlerName' to 'HandlerName_handler'
    PreHandler = bitstring_to_list(HandlerName),
    %% replaces '-' with '_', (so much faster than re:replace/3),
    %% and convert to atom.
    Handler = list_to_atom(string:concat(string:join(
        string:tokens(PreHandler, "-"), "_"), "_handler")),
    %% WARNING NOTE: the 'Handler' module has to be loaded, fortunately
    %%          the framework does it for us at boot time.
    %% checks if the handler implements a 'init/2' function.
    case erlang:function_exported(Handler, init, 2) of
        false ->
            send_error_404(Req);
        true ->
            %% converts the action string.
            PreAction = bitstring_to_list(ActionName),
            %% replaces '-' with '_'. and convert to atom.
            Action = list_to_atom((string:join(
                string:tokens(PreAction, "-"), "_"))),
            %% checks if the handler implement at least one of
            %% 'Action/3' or 'Action/4' function.
            case (erlang:function_exported(Handler, Action, 3)) or
                (erlang:function_exported(Handler, Action, 4))
            of
                false ->
                    send_error_404(Req);
                true ->
                    debug_logger:log_info_msg(Req),

                    %% redirects the cowboy_handler to the actual handler
                    %% and action.
                    Env2 = lists:keydelete(handler, 1, Env),
                    Env3 = lists:keydelete(handler_opts, 1, Env2),
                    Env4 = [{handler, Handler}, {handler_opts,
                        [{action, Action}]} | Env3],
                    {ok, Req, Env4}
            end
    end.

%% sends error '404 not found' to the client.
send_error_404(Req) ->
    Req2 = cowboy_req:reply(404, Req),
    {stop, Req2}.