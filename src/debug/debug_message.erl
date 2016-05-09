%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% This modules handles the error and logs messages for the system.
%%% It depends on 'app_error' template because it use it to render
%%% the error to be shown later to the client.
%%% @end
%%% Created : 14. Jul 2015 6:06 PM
%%%-------------------------------------------------------------------
-module(debug_message).
-author("Ivan Carmenates Garcia").

%%%-------------------------------------------------------------------
%%% API Exports
%%%-------------------------------------------------------------------
-export([
    render_error/2,
    render_server_error/2]).

%%%-------------------------------------------------------------------
%%% TYPE Definition
%%%-------------------------------------------------------------------
-type error_type() :: atom().
-type error_list() :: list().
-type error() :: {error_type(), error_list()}.

-export_type([error/0]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%% @doc
%% Renders a html body for the given errors using the
%% 'debug_message_dtl' template.
%%
%% Example:
%%   {login_errors, ["unauthorized"]}
%% @end
-spec render_error(Error, Req) -> Req2 when
    Error :: error(),
    Req :: cowboy_req:req(),
    Req2 :: cowboy_req:req().
render_error(Error, Req) ->
    {ok, Body} = debug_message_dtl:render([Error]),
    cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        Body, Req).

%% @doc
%% Renders a html body for the given internal server errors using the
%% 'debug_message_dtl' template.
%%
%% Example:
%%   {session_errors, ["no_connected_db"]}
%% @end
-spec render_server_error(Error, Req) -> Req2 when
    Error :: error(),
    Req :: cowboy_req:req(),
    Req2 :: cowboy_req:req().
render_server_error(Error, Req) ->
    {ok, Body} = debug_message_dtl:render([Error]),
    cowboy_req:reply(500,
        [{<<"content-type">>, <<"text/html">>}],
        Body, Req).