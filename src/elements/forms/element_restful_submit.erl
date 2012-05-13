% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Steffen Panning
% See MIT-LICENSE for licensing information.

-module (element_restful_submit).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, restful_submit).

render_element(Record) ->
    ID = Record#restful_submit.id,
    Anchor = Record#restful_submit.anchor,
    case Record#restful_submit.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=click, validation_group=ID, postback=Postback, delegate=Record#restful_submit.delegate })
    end,

    Value = ["  ", wf:html_encode(Record#restful_submit.text, Record#restful_submit.html_encode), "  "], 
    wf_tags:emit_tag(input, [
        {type,  submit},
	{name, Record#restful_submit.html_name},
        {class, [restful_submit, Record#restful_submit.class]},
        {style, Record#restful_submit.style},
        {value, Value}
    ]).
