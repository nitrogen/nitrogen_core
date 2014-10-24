% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_mark).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, mark).

-spec render_element(#mark{}) -> body().
render_element(Record = #mark{}) ->
    Text = wf_convert:html_encode(Record#mark.text, Record#mark.html_encode),
    wf_tags:emit_tag(mark, [Text, Record#mark.body], [
        {id, Record#mark.html_id},
        {class, ["mark", Record#mark.class]},
        {title, Record#mark.title},
        {style, Record#mark.style},
        {data_fields, Record#mark.data_fields}
    ]).
