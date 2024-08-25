% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_js_custom).
-include("wf.hrl").
-export([render_action/1]).

render_action(Record) -> 
    Target = Record#js_custom.target,
    Text = wf:js_escape(Record#js_custom.text),
    Function = Record#js_custom.function,
    Args = Record#js_custom.args,
    WhenEmpty = Record#js_custom.when_empty,
    Opts = #{
        function=>Function,
        args=>Args,
        when_empty=>WhenEmpty
    },
    validation_handler:js_add_validator(Target, custom, Text, Opts).
