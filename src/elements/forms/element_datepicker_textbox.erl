% vim: sw=4 ts=4 et ft=erlang
%%% Datepicker Control Element.
%%% Copyright (c) 2009 Torbjorn Tornkvist
%%% See MIT-LICENSE for the Nitrogen Web Framework for Erlang

-module (element_datepicker_textbox).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, datepicker_textbox).

-spec render_element(#datepicker_textbox{}) -> body().
render_element(Record) -> 
    Anchor = Record#datepicker_textbox.anchor,
    Options = action_jquery_effect:options_to_js(Record#datepicker_textbox.options),

    Textbox = wf_util:copy_fields(Record, #textbox{}),
    Textbox1 = Textbox#textbox{class=[datepicker_textbox, Textbox#textbox.class]},

    Script = wf:f("Nitrogen.$datepicker(obj('~s'), ~s);", [Anchor, Options]),
    wf:wire(Anchor, #script { script=Script }),

    element_textbox:render_element(Textbox1).
