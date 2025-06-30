% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.
%
% This handler is powered by the nitro_secret dependency:
% https://github.com/nitrogen/nitro_secret

-module(default_secret_handler).
-include("wf.hrl").
-behaviour(secret_handler).
-export ([
    init/2,
    finish/2,
    get_value/4
]).

init(_Config, State) ->
    {ok, State}.

finish(_Config, _State) ->
    {ok, []}.

get_value(Key, Default, _Config, _State) ->
    nitro_secret:get(Key, Default).
