% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2011 Sergei Lebedev
% See MIT-LICENSE for licensing information.

-module (element_pre).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, pre).

-spec render_element(#pre{}) -> body().
render_element(Record) ->
    Body = wf:html_encode(Record#pre.text, Record#pre.html_encode),
    wf_tags:emit_tag(pre, Body, [
        {class, [pre, Record#pre.class]},
        {title, Record#pre.title},
        {data_fields, Record#pre.data_fields},
        {style, Record#pre.style}
    ]).
