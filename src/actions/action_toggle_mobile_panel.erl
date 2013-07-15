% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2013 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(action_toggle_mobile_panel).
-include("wf.hrl").
-export([
    reflect/0,
    render_action/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, toggle_mobile_panel).

-spec render_action(#toggle_mobile_panel{}) -> script().
render_action(#toggle_mobile_panel{target=Target}) ->
    wf:f(<<"objs('~s').panel(\"toggle\");">>,[Target]).
