% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(default_example_handler).
-include("wf.hrl").
-behaviour(example_handler).
-export ([
    init/2,
    finish/2,
    something_that_changes_handler_state/4,
    something_that_does_not_change_handler_state/3
]).

init(_Config, State) ->
    {ok, State}.

finish(_Config, _State) ->
    {ok, []}.

something_that_changes_handler_state(Arg1, Arg2, Config, State) ->
    RetVal = {Arg1, Arg2}, %% just doing something arbitrary with the passed args
    {ok, RetVal, State}.

something_that_does_not_change_handler_state(Arg1, Config, State) ->
    ok.
