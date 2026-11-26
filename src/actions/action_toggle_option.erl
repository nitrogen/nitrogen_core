% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2019 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (action_toggle_option).
-include("wf.hrl").
-export([render_action/1]).

render_action(#disable_option{target=Target, value=Value}) -> 
    toggle_option(true, Target, Value);
render_action(#enable_option{target=Target, value=Value}) ->
    toggle_option(false, Target, Value).

toggle_option(Disabled, Target, Value) ->
    wf:f("objs('~s').find(\"option[value='~ts']\").prop('disabled',~p);", [Target, wf:js_escape(wf:to_list(Value)), Disabled]).
