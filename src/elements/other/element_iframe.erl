% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_iframe).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, iframe).

-spec render_element(#iframe{}) -> body().
render_element(Record) ->
    Attributes = [
        {id, Record#iframe.html_id},
        {class, [iframe, Record#iframe.class]},
        {style, Record#iframe.style},
        {data_fields, Record#iframe.data_fields},
        {aria, Record#iframe.aria},
        {height, Record#iframe.height},
        {width, Record#iframe.width},
        {src, Record#iframe.src},
        {srcdoc, Record#iframe.srcdoc},
        {align, Record#iframe.align},
        {frameborder, Record#iframe.frameborder},
        {name, Record#iframe.name},
        {sandbox, Record#iframe.sandbox},
        {seamless, Record#iframe.seamless},
        {allowfullscreen, Record#iframe.allowfullscreen}               
    ],

    wf_tags:emit_tag(iframe, [], Attributes).
