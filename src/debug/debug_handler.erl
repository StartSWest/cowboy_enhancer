%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2015 5:23 AM
%%%-------------------------------------------------------------------
-module(debug_handler).
-author("Ivan Carmenates Garcia").

%%-------------------------------------------------------------------------------------------------
%% Callback Exports
%%-------------------------------------------------------------------------------------------------
-export([
    init/2,
    terminate/3,
    ensure_logged_in/2,
    init/3]).

%%-------------------------------------------------------------------------------------------------
%% Actions Exports
%%-------------------------------------------------------------------------------------------------
-export([
    observer/4,
    rebuild_all/4]).

%%-------------------------------------------------------------------------------------------------
%% Callback Functions
%%-------------------------------------------------------------------------------------------------

init(Req, Opts) ->
    {require_auth, Req, Opts}.

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Ensures that the user gets authenticated.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec ensure_logged_in(Req, Opts) ->
    {true, SessionID} | {ok, Req2, Opts2} | {stop, Req2} when
    Req :: cowboy_req:req(),
    Opts :: any(),
    SessionID :: binary(),
    Req2 :: cowboy_req:req(),
    Opts2 :: any().
ensure_logged_in(Req, Opts) ->
    session_handler:ensure_logged_in(Req, Opts).

%%-------------------------------------------------------------------------------------------------
%% @doc
%% Start point of the request handler when user got authenticated.
%% @end
%%-------------------------------------------------------------------------------------------------
-spec init(Req, Opts, SessionID) ->
    {ok, Req2, Opts} | {stop, Req2} | {suspend, Module, Function, Arguments} when
    Req :: cowboy_req:req(),
    Opts :: any(),
    SessionID :: session_manager:session_id(),
    Req2 :: cowboy_req:req(),
    Module :: atom(),
    Function :: atom(),
    Arguments :: any().
init(Req, Opts, SessionID) ->
    request_manager:handle_req(?MODULE, Req, Opts, SessionID).

%%-------------------------------------------------------------------------------------------------
%% Request Actions
%%-------------------------------------------------------------------------------------------------

observer(<<"GET">>, Req, Opts, SessionID) ->
    R = case session_controller:get_session_data_from_session(SessionID) of
        #{username := <<"ivan">>} ->
            observer:start(),
            "ok";
        _ ->
            "not_authorized"
    end,
    Req2 = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        R, Req),
    {ok, Req2, Opts}.

rebuild_all(<<"GET">>, Req, Opts, Session) ->
    R = case session_controller:get_session_data_from_session(Session) of
        #{username := <<"ivan">>} ->
            ce_control:rebuild_all();
        _ ->
            "not_authorized"
    end,
    Req2 = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        io_lib:format("~p", [R]), Req),
    {ok, Req2, Opts}.

terminate(_Reason, _Req, _State) ->
    ok.