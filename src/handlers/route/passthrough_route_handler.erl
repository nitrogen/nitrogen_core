% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (passthrough_route_handler).
-behaviour (route_handler).
-include("wf.hrl").
-export ([
    init/2, 
    finish/2
]).

init(Module, State) -> 
    % Some values...
    Bridge = wf_context:bridge(),
    Path = sbw:path(Bridge),

    % Update the page_context with the path and module.
    wf_context:page_module(Module),
    wf_context:path_info(Path),
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.
