% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Steffen Panning
% See MIT-LICENSE for licensing information.

-module (element_restful_submit).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, restful_submit).

-spec render_element(#restful_submit{}) -> body().
render_element(Record) ->
    Value = wf:html_encode(Record#restful_submit.text, Record#restful_submit.html_encode),
    UniversalAttributes = [
        {type,  submit},
        {name, Record#restful_submit.html_name},
        {class, ?ADD_ELEMENT_CLASS(restful_submit, Record#restful_submit.class)},
        {title, Record#restful_submit.title},
        {style, Record#restful_submit.style},
        {data_fields, Record#restful_submit.data_fields},
        {aria, Record#restful_submit.aria}
    ],
    case Record#restful_submit.body of 
        [] ->   
            wf_tags:emit_tag(input, [{value, Value} | UniversalAttributes]);
        Body ->
            wf_tags:emit_tag(button, [Body, Value], UniversalAttributes)
    end.

