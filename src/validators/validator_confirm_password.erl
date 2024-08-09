% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(validator_confirm_password).
-include("wf.hrl").
-export([
    render_action/1
]).

render_action(Record)  ->
    #confirm_same{
        trigger=Record#confirm_password.trigger,
        target=Record#confirm_password.target,
        text=Record#confirm_password.text,
        confirm_id=Record#confirm_password.password,
        attach_to=Record#confirm_password.attach_to
    }.
