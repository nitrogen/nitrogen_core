% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_html5_header).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, html5_header).

-spec render_element(#html5_header{}) -> body().
render_element(Record) ->
    wf_tags:emit_tag('header', Record#html5_header.body, [
        {id, Record#html5_header.html_id},
        {class, ["html5_header", Record#html5_header.class]},
        {title, Record#html5_header.title},
        {style, Record#html5_header.style},
        {role, Record#html5_header.role},
        {data_fields, Record#html5_header.data_fields}
    ]).
