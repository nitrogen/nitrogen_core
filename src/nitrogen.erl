% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(nitrogen).
-behaviour(simple_bridge_callout).
-export([
        init_request/2,
        init_request/1,
        handler/2,
        run/0
    ]).

%% Simple Bridge Callout functions
-export([
        run/1,
        ws_init/1,
        ws_message/2,
        ws_info/2,
        ws_terminate/2
    ]).
    


%% init_request/2 kept for backwards compatibility, but is no longer needed
init_request(Bridge, _) ->
    init_request(Bridge).

init_request(Bridge) ->
    wf_context:init_context(Bridge).

handler(Module, Config) ->
    wf_handler:set_handler(Module, Config).

run() -> 
    wf_core:run().


run(Bridge) ->
    init_request(Bridge),
    nitrogen_callout:run().

ws_init(Bridge) ->
    init_request(Bridge),
    ok.

ws_message({text, _Msg}, _Bridge) ->
    noreply.

ws_info(_Msg, _Bridge) ->
    noreply.

ws_terminate(_Reason, _Bridge) ->
    close.
