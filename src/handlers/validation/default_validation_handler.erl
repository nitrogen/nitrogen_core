% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module(default_validation_handler).
-behaviour(validation_handler).
-include("wf.hrl").
-export([
    init/2,
    finish/2,
    attach/5,
    validate/4
]).


init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

validate(_Config, State, Field, Value) ->
    true.

attach(_Config, State, Targetid, Field, Validators) ->
    State.
