% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_image).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, image).

-spec render_element(#image{}) -> body().
render_element(Record) ->
    Attributes = [
        {id, Record#image.html_id},
        {class, [image, Record#image.class]},
        {style, Record#image.style},
        {data_fields, Record#image.data_fields},
        {height, Record#image.height},
        {width, Record#image.width},
        {alt, Record#image.alt},
        {src, wf:to_binary(Record#image.image)}
    ],

    wf_tags:emit_tag(img, Attributes).
