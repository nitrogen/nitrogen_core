% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Steffen Panning
% See MIT-LICENSE for licensing information.

-module (element_restful_reset).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, restful_reset).

-spec render_element(#restful_reset{}) -> body().
render_element(Record) ->
    Value = ["  ", wf:html_encode(Record#restful_reset.text, Record#restful_reset.html_encode), "  "], 
    wf_tags:emit_tag(input, [
        {type,  reset},
        {name,  Record#restful_reset.html_name},
        {class, ?ADD_ELEMENT_CLASS(restful_reset, Record#restful_reset.class)},
        {title, Record#restful_reset.title},
        {style, Record#restful_reset.style},
        {value, Value},
        {data_fields, Record#restful_reset.data_fields},
        {aria, Record#restful_reset.aria}
    ]).
