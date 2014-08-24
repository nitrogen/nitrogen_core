% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_nav).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, nav).

-spec render_element(#nav{}) -> body().
render_element(Record ) ->
    wf_tags:emit_tag(nav, Record#nav.body, [
        {id, Record#nav.html_id},
        {class, ["nav", Record#nav.class]},
        {title, Record#nav.title},
        {style, Record#nav.style},
        {data_fields, Record#nav.data_fields}
    ]).
