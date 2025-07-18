% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(action_modal).
-include("wf.hrl").
-export([render_action/1]).

-export([reflect/0]).
reflect() -> record_info(fields, modal).

render_action(Rec = #modal{}) ->
    modal_handler:render_open_action(Rec);
render_action(Rec = #close_modal{}) ->
    modal_handler:render_close_action(Rec).
