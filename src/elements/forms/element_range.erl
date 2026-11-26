%$ vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_range).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, range).

-spec render_element(#range{}) -> body().
render_element(Record=#range{}) -> 
    ID = Record#range.id,
    Anchor = Record#range.anchor,
    action_event:maybe_wire_next(Anchor, Record#range.next),

    case Record#range.postback of
        undefined -> ignore;
        Postback ->
            Event = #event{
                type=change,
                postback=Postback,
                vessel=Record#range.vessel,
                validation_group=ID,
                delegate=Record#range.delegate
            },
            wf:wire(Anchor, Event)
    end,

    wf_tags:emit_tag(input, [
        {type, range}, 
        {class, ?ADD_ELEMENT_CLASS(range, Record#range.class)},
        {title, Record#range.title},
        {min, Record#range.min},
        {max, Record#range.max},
        {step, Record#range.step},
        {style, Record#range.style},
        {id, Record#range.html_id},
        {value, Record#range.value},
        {data_fields, Record#range.data_fields},
        {aria, Record#range.aria}
    ]).
