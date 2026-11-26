% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (default_postback_handler).
-behaviour (postback_handler).
-include("wf.hrl").
-export ([
    init/2,
    finish/2,
    postback_request/2
]).


init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

postback_request(_Config, _State) ->
    ok.
