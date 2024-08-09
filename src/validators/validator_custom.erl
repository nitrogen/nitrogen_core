% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_custom).
-include("wf.hrl").
-compile(export_all).

render_action(Record) -> 
    TriggerPath = Record#custom.trigger,
    TargetPath = Record#custom.target,
    wf_validation:register_validator(TriggerPath, TargetPath, Record),
    [].
