%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2015 10:13 PM
%%%-------------------------------------------------------------------
-module(security_lib).
-author("Ivan Carmenates Garcia").

-behaviour(gen_server).

%%%-------------------------------------------------------------------
%%% API Exports
%%%-------------------------------------------------------------------
-export([
    start_link/0]).

-export([
    generate_unique_id/0,
    generate_unique_hash_id/0,
    hash_value/1,
    hash_random/1,
    hash_password/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%%%-------------------------------------------------------------------
%%% MACRO Definition
%%%-------------------------------------------------------------------
-define(SERVER, ce_security_lib).
-define(SECRET_WORD, "7gDfgd-|3IV=anco34g#@fge/rSoF+tw(are#21Cor*Por)aTion!").

%% -------------------------------------------------------------------
%% RECORD Definition
%% -------------------------------------------------------------------
-record(state, {}).

%% -------------------------------------------------------------------
%% API Functions
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc
%% Generates an unique ID in the system.
%% @end
%% -------------------------------------------------------------------
-spec generate_unique_id() -> UniqueID when
    UniqueID :: string().
generate_unique_id() ->
    NowNumber = lists:concat(tuple_to_list(erlang:timestamp())),
    RandomNumber = integer_to_list(gen_server:call(?SERVER, {random_strong_uniform, 10000000}, infinity)),
    NowNumber ++ RandomNumber.

%% -------------------------------------------------------------------
%% @doc
%% Generates a hashed value for a generated unique ID in the system.
%% @end
%% -------------------------------------------------------------------
-spec generate_unique_hash_id() -> UniqueHashID when
    UniqueHashID :: string().
generate_unique_hash_id() ->
    hash_value(generate_unique_id()).

%% -------------------------------------------------------------------
%% @doc
%% Generates a hashed value for the specified value.
%% @end
%% -------------------------------------------------------------------
-spec hash_value(Value) -> HashedValue when
    Value :: string(),
    HashedValue :: binary().
hash_value(Value) ->
    <<HashedValue:160/integer>> = crypto:hash(sha, Value),
    list_to_bitstring(lists:flatten(io_lib:format("~40.16.0b", [HashedValue]))).

%% -------------------------------------------------------------------
%% @doc
%% Generates a hashed value for the specified value using a
%% random salt number.
%% @end
%% -------------------------------------------------------------------
-spec hash_random(Value) -> HashedRandomValue when
    Value :: string(),
    HashedRandomValue :: binary().
hash_random(Value) ->
    Salt = generate_unique_id(),
    SaltedValue = Value ++ Salt,
    <<HashedRandomValue:160/integer>> = crypto:hmac(sha, Salt, SaltedValue),
    list_to_bitstring(lists:flatten(io_lib:format("~40.16.0b", [HashedRandomValue]))).

%% -------------------------------------------------------------------
%% @doc
%% Generates a hashed password for the specified password.
%% @end
%% -------------------------------------------------------------------
-spec hash_password(Password) -> HashedPassword when
    Password :: string() | binary(),
    HashedPassword :: binary().
hash_password(Password) ->
    %% TODO: do better implementation.
    <<HashedRandomValue:160/integer>> = crypto:hmac(sha, ?SECRET_WORD, Password),
    list_to_bitstring(lists:flatten(io_lib:format("~40.16.0b", [HashedRandomValue]))).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, infinity}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    quickrand:seed(),
    {ok, #state{}}.

handle_call({random_uniform, N}, _From, State) ->
    {reply, quickrand:uniform(N), State};
handle_call({random_strong_uniform, N}, _From, State) ->
    {reply, quickrand:strong_uniform(N), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.