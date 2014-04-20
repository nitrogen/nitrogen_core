% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textarea).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render_element(Record) -> 
    Anchor = Record#textarea.anchor,
    Text = html_encode(Record#textarea.text, Record#textarea.html_encode),
    Placeholder  = wf:html_encode(Record#textarea.placeholder, true),
    action_event:maybe_wire_next(Anchor, Record#textarea.next),
    wf_tags:emit_tag(textarea, Text, [
        {class, [textarea, Record#textarea.class]},
        {id, Record#textarea.html_id},
        {style, Record#textarea.style},
        {title, Record#textarea.title},
        {name, Record#textarea.html_name},
        {cols, Record#textarea.columns},
        {rows, Record#textarea.rows},
        ?WF_IF(Record#textarea.disabled,disabled,undefined),
        ?WF_IF(Record#textarea.readonly,readonly,undefined),
        {placeholder, Placeholder},
        {data_fields, Record#textarea.data_fields}
    ]).

html_encode(L, false) -> wf:to_list(lists:flatten([L]));
html_encode(L, true) -> html_encode(wf:to_list(lists:flatten([L]))).    
html_encode([]) -> [];
html_encode([H|T]) ->
    case H of
        $< -> "&lt;" ++ html_encode(T);
        $> -> "&gt;" ++ html_encode(T);
        $" -> "&quot;" ++ html_encode(T);
        $' -> "&#39;" ++ html_encode(T);
        $& -> "&amp;" ++ html_encode(T);
        % $\n -> "<br>" ++ html_encode(T);
        _ -> [H|html_encode(T)]
    end.
