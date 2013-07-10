% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2013 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_h).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, h).

-spec render_element(H :: #h1{} | #h2{} | #h3{} | #h4{} | #h5{} | #h6{} | #h{}) -> html().
render_element(H) when  is_record(H,h1); is_record(H,h2);
                        is_record(H,h3); is_record(H,h4);
                        is_record(H,h5); is_record(H,h6) ->
    render_element(setelement(1,H,h));
render_element(Rec) ->
    Text = wf:html_encode(Rec#h.text, Rec#h.html_encode),
    Body = Rec#h.body,
    Tag = tag(Rec#h.size),
    wf_tags:emit_tag(Tag, [Text, Body], [
        {id, Rec#h.html_id},
        {class, [Tag, Rec#h.class]},
        {style, Rec#h.style}
    ]).

tag(1) -> h1;
tag(2) -> h2;
tag(3) -> h3;
tag(4) -> h4;
tag(5) -> h5;
tag(6) -> h6;
tag(X) -> throw({error, {invalid_size_for_h_element, X}}).

