% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module(element_time).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, time).

-spec render_element(#time{}) -> body().
render_element(Record) ->
    Text = wf:html_encode(Record#time.text, Record#time.html_encode),
    wf_tags:emit_tag(time, [Text, Record#time.body], [
        {id, Record#time.html_id},
        {class, ["time", Record#time.class]},
        {title, Record#time.title},
        {style, Record#time.style},
        {role, Record#time.role},
        {data_fields, Record#time.data_fields},
        {aria, Record#time.aria},
        ?WF_IF(Record#time.datetime, {datetime, Record#time.datetime})
    ]).
