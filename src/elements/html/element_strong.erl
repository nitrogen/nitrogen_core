% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2011 Sergei Lebedev
% See MIT-LICENSE for licensing information.

-module (element_strong).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, strong).

-spec render_element(#strong{}) -> body().
render_element(Record) ->
    Body = [
        wf:html_encode(Record#strong.text, Record#strong.html_encode),
        Record#strong.body
    ],
    wf_tags:emit_tag(strong, Body, [
        {class, [p, Record#strong.class]},
        {title, Record#strong.title},
        {style, Record#strong.style},
        {data_fields, Record#strong.data_fields}
    ]).
