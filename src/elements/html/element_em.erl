% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2011 Sergei Lebedev
% See MIT-LICENSE for licensing information.

-module (element_em).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, em).

-spec render_element(#em{}) -> body().
render_element(Record ) ->
    Body = [
        wf:html_encode(Record#em.text, Record#em.html_encode),
        Record#em.body
    ],
    wf_tags:emit_tag(em, Body, [
        {class, [p, Record#em.class]},
        {title, Record#em.title},
        {title, Record#em.title},
        {style, Record#em.style},
        {data_fields, Record#em.data_fields}
    ]).
