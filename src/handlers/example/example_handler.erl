% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(example_handler).
-include("wf.hrl").
-export ([
    something_that_changes_handler_state/2
    something_that_does_not_change_handler_state/1
]).

-callback init(         handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback finish(       handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback something_that_changes_handler_state(Arg1 :: term(),
                        Arg2 :: term(),
                        handler_config(),
                        handler_state()) -> {ok, RetVal :: term(), handler_state()}.
-callback something_that_does_not_change_handler_state(Arg1 :: term(),
                        handler_config(),
                        handler_state()) -> term().

%% behaviors that return a potentially new handler state will have to return either:
%% {ok, State}
%% {ok, RetVal, State}
%% {ok, RetVal1, RetVal2, State}
%% The return value from wf_handler:call will then be (respective to the above)
%% ok
%% {ok, RetVal}
%% {ok, RetVal1, RetVal2}

-spec something_that_changes_handler_state(Arg1 :: term(), Arg2 :: term()) -> {ok, term()}.
something_that_changes_handler_state(Arg1, Arg2) ->
    {ok, _Value} = wf_handler:call(example_handler, something_that_changes_handler_state, [Arg1, Arg2]).


%% behavior functions that don't change handler state just return a value.
something_that_does_not_change_handler_state(Arg1) ->
    _Value = wf_handler:call_readonly(example_handler, something_that_does_not_change_handler_state, [Arg1]).
