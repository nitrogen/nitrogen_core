% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_validation_error).
-include("wf.hrl").
-export([render_action/1]).

render_action(Record) -> 
    TargetPath = Record#validation_error.target,
    AttachTo = Record#validation_error.attach_to,
    Text = Record#validation_error.text,

    AttachToObj = ?WF_IF(?WF_BLANK(AttachTo), null, #js_fun{function="obj", args=[AttachTo]}),

    #js_fun{function="Nitrogen.$validation_error", args=[TargetPath, AttachToObj, Text]}.

%%    Opts = #{
%%        function=>"Nitrogen.$return_false",
%%        args=>"[]",
%%        when_empty=>true
%%    },
%%    %% The process here is perhaps a little round-about, but it allows us to do something 
%%    [
%%        validation_handler:js_constructor(TargetPath, "", "", submit, AttachTo),
%%        validation_handler:js_add_validator(TargetPath, custom, Text, Opts),
%%        wf:f(<<"Nitrogen.$validate_element('~s', '');">>, [TargetPath])
%%%        <<"v.validate();">>
%%%        wf:f(<<"var v = new LiveValidation(obj('~ts'), { onlyOnSubmit: true ~s});">>, [TargetPath, InsertAfterNode]),
%%%        wf:f(<<"v.add(Validate.Custom, { against: Nitrogen.$return_false, failureMessage: \"~ts\", displayMessageWhenEmpty: true });">>, [Text]),
%%%        <<"v.validate();">>
%%    ].
