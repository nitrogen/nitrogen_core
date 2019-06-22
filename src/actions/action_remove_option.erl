% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2019 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (action_remove_option).
-include("wf.hrl").
-export([render_action/1]).

render_action(#remove_option{target=Target, value=Value}) ->
    wf:f("objs('~s').find(\"option[value='~ts']\").remove();", [Target, wf:js_escape(wf:to_list(Value))]).
