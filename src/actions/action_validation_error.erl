% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_validation_error).
-include("wf.hrl").
-export([render_action/1]).

render_action(Record) -> 
    TargetPath = Record#validation_error.target,
    InsertAfterNode = insert_after_node(Record#validation_error.attach_to),
    Text = wf:js_escape(Record#validation_error.text),
    [
        %% Is this going to be a problem with memory leaks in the browser? I'm
        %% not sure. I'm not too worried about it though, since I'd like to
        %% replace LiveValidation altogether.
        wf:f(<<"var v = new LiveValidation(obj('~ts'), { onlyOnSubmit: true ~s});">>, [TargetPath, InsertAfterNode]),
        wf:f(<<"v.add(Validate.Custom, { against: Nitrogen.$return_false, failureMessage: \"~ts\", displayMessageWhenEmpty: true });">>, [Text]),
        <<"v.validate();">>
    ].

insert_after_node(undefined) ->
    "";
insert_after_node(Node) ->
    wf:f(<<", insertAfterWhatNode : obj(\"~s\")">>, [Node]).
