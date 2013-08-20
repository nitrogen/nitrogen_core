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
    Value = ["  ", wf:html_encode(Record#restful_submit.text, Record#restful_submit.html_encode), "  "], 
    wf_tags:emit_tag(input, [
        {type,  submit},
        {name, Record#restful_submit.html_name},
        {class, [restful_submit, Record#restful_submit.class]},
        {style, Record#restful_submit.style},
        {data_fields, Record#restful_submit.data_fields},
        {value, Value}
    ]).
