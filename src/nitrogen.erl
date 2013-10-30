% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen).
-export ([
	init_request/2,
    init_request/1,
	handler/2,
	run/0]).

%% init_request/2 kept for backwards compatibility, but is no longer needed
init_request(Bridge, _) ->
    init_request(Bridge).

init_request(Bridge) ->
    wf_context:init_context(Bridge).

handler(Module, Config) ->
    wf_handler:set_handler(Module, Config).

run() -> 
    wf_core:run().
