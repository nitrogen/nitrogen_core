% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(action_confirm).
-include("wf.hrl").
-export([render_action/1]).

render_action(Record) -> 
    TriggerPath = Record#confirm.trigger,
    TargetPath = Record#confirm.target,
    Delegate = Record#confirm.delegate,
    [
        wf:f("if (confirm(\"~ts\")) {", [wf:js_escape(Record#confirm.text)]),
        #event{
            postback=Record#confirm.postback,
            vessel=Record#confirm.vessel,
            trigger=TriggerPath,
            target=TargetPath,
            delegate=Delegate
        },
        Record#confirm.actions,
        "}"
    ].

