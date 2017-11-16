%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%% Provides http error codes nice format
%%% @end
%%% Created : 07. Jul 2015 2:27 PM
%%%-------------------------------------------------------------------
-module(debug_hooks).
-author("Ivan Carmenates Garcia").

%%-------------------------------------------------------------------------------------------------
%% API Exports
%%-------------------------------------------------------------------------------------------------
-export([error_hook/4]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
    Error = maps:get(Code, http_errors(), #{description => "unknown", name => "unknown"}),
    #{description := Description, name := Name} = Error,
    Body = ["<div style=\"background: rgb(254, 255, 207); margin: 20px; padding-right: 50px; padding-bottom: 100px\">"
    "<p style=\"font-style: normal; font-family: cursive; font-size: 24px; font-stretch: ultra-condensed;
      font-weight: bold; color: red; padding-left: 100px; padding-top: 80px;\">"
    "HTTP Error " ++ integer_to_list(Code) ++ ": " ++ Name, "</p>"
    "<p style=\"font-family: calibri; font-size: 22px; padding-left: 100; margin-top: -10px; font-weight: bold;\">Description:</p>"
    "<p style=\"font-family: calibri; font-size: 20px; padding-left: 100; margin-top: -14;\">" ++ Description ++ "</p></div>"],
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(iolist_size(Body))}),
    cowboy_req:reply(Code, Headers2, Body, Req);
error_hook(_Code, _Headers, _Body, Req) ->
    Req.

http_errors() ->
    #{
        400 => #{
            name => "Bad Request",
            description => "Cowboy will send this status code for any of the following reasons:\n"
            " * Too many empty lines were sent before the request.\n"
            " * The request-line could not be parsed.\n"
            " * Too many headers were sent.\n"
            " * A header name was too long.\n"
            " * A header value was too long.\n"
            " * The host header was missing from an HTTP/1.1 request.\n"
            " * The host header could not be parsed.\n"
            " * The requested host was not found.\n"
            " * The requested path could not be parsed.\n"
            " * The accept header could not be parsed when using REST.\n"
            " * REST under normal conditions.\n"
            " * A Websocket upgrade failed.\n"},
        401 => #{
            name => "Unauthorized",
            description => "This status code is sent by `cowboy_rest`.\n"},
        403 => #{
            name => "Forbidden",
            description => "This status code is sent by `cowboy_rest`.\n"},
        404 => #{
            name => "Not Found",
            description => "This status code is sent when the router successfully\n"
            "resolved the host but didn't find a matching path for\n"
            "the request. It may also be sent by `cowboy_rest` under\n"
            "normal conditions.\n"},
        405 => #{
            name => "Method Not Allowed",
            description => "This status code is sent by `cowboy_rest`.\n"},
        406 => #{
            name => "Not Acceptable",
            description => "This status code is sent by `cowboy_rest`.\n"},
        408 => #{
            name => "Request Timeout",
            description => "Cowboy will send this status code to the client if the\n"
            "client started to send a request, indicated by the\n"
            "request-line being received fully, but failed to send\n"
            "all headers in a reasonable time.\n"},
        409 => #{
            name => "Conflict",
            description => "This status code is sent by `cowboy_rest`.\n"},
        410 => #{
            name => "Gone",
            description => "This status code is sent by `cowboy_rest`.\n"},
        412 => #{
            name => "Precondition Failed",
            description => "This status code is sent by `cowboy_rest`.\n"},
        413 => #{
            name => "Request Entity Too Large",
            description => "This status code is sent by `cowboy_rest`.\n"},
        414 => #{
            name => "Request-URI Too Long",
            description => "Cowboy will send this status code to the client if the\n"
            "request-line is too long. It may also be sent by\n"
            "`cowboy_rest` under normal conditions.\n"},
        415 => #{
            name => "Unsupported Media Type",
            description => "This status code is sent by `cowboy_rest`.\n"},
        500 => #{
            name => "Internal Server Error",
            description => "This status code is sent when a crash occurs in HTTP, loop\n"
            "or REST handlers, or when an invalid return value is\n"
            "returned. It may also be sent by `cowboy_rest` under\n"
            "normal conditions.\n"},
        501 => #{
            name => "Not Implemented",
            description => "This status code is sent by `cowboy_rest`.\n"},
        503 => #{
            name => "Service Unavailable",
            description => "This status code is sent by `cowboy_rest`.\n"},
        505 => #{
            name => "HTTP Version Not Supported",
            description => "Cowboy only supports the versions 1.0 and 1.1 of HTTP.\n"
            "In all other cases this status code is sent back to the\n"
            "client and the connection is closed.\n"}
    }.