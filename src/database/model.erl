%%%-------------------------------------------------------------------
%%% @author Ivan Carmenates Garcia
%%% @copyright (C) 2015, Ivanco Software Corporation.
%%% @doc
%%% Cowboy enhancer database model behavior.
%%% @end
%%% Created : 05. Sep 2015 11:40 AM
%%%-------------------------------------------------------------------
-module(model).
-author("Ivan Carmenates Garcia").

-callback validation_tests(ModelDataMap) ->
    [{Tag, [{AssertionFunction, FailAssertionMessage}, ...]}, ...] when
    ModelDataMap :: map(),
    Tag :: atom(),
    AssertionFunction :: fun(),
    FailAssertionMessage :: term().