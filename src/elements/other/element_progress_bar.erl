% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2014 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_progress_bar).
-include("wf.hrl").
-export([
	reflect/0,
	render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, progress_bar).

-spec render_element(#progress_bar{}) -> html().
render_element(Rec = #progress_bar{value=Value, color=Color, max=Max, label=Label} ) ->
    LabelElement = render_label(Label, Value, Max),
    Tempid = wf:temp_id(),
    Class = create_class(Tempid, Label, Rec#progress_bar.class),
    wire_init(Tempid, Value, Max, Color),
    wf_tags:emit_tag('div', LabelElement, [
        {style, Rec#progress_bar.style},
        {title, Rec#progress_bar.title},
        {id, Rec#progress_bar.html_id},
        {class, Class},
        {data_fields, Rec#progress_bar.data_fields},
        {aria, Rec#progress_bar.aria}
    ]).

create_class(Tempid, Label, CurClass) ->
    %% This is is the class we'll use to identify our in initialization
    Classid = wf_render_elements:normalize_id(Tempid),

    %% This will be an identifier class which nitrogen.js will use to determine
    %% how to format the label.
    LabelClass = label_class(Label),

    [Classid, LabelClass | CurClass].


wire_init(ID, Val, Max, Color) ->
    RenderedValue = render_value(Val),
    wf:wire(wf:f("Nitrogen.$init_progress_bar('~s', ~p, ~p, '~s');", [ID, RenderedValue, Max, Color])).

render_value(undefined) ->
    false;
render_value(V) ->
    V.


label_class(undefined) ->
    'progressbar-label-none';
label_class(Atom) when is_atom(Atom) -> 
    wf:to_atom(wf:to_list(['progressbar-label-',Atom]));
label_class(Str) when is_list(Str); is_binary(Str) -> 
    'progressbar-label-string'.


render_label(Label, undefined, _Max) when is_atom(Label) ->
    [];
render_label(undefined, _Val, _Max) ->
    [];
render_label(percent, Val, Max) when Val=/=undefined ->
    #panel{class=['progressbar-label'], text=render_percent(Val, Max)};
render_label(ratio, Val, Max) when Val=/=undefined -> 
    #panel{class=['progressbar-label'], text=render_ratio(Val, Max)};
render_label(both, Val, Max) when Val=/=undefined ->
    #panel{class=['progressbar-label'], text=render_both(Val, Max)};
render_label(String, _, _) when is_list(String); is_binary(String) ->
    #panel{class=['progressbar-label'], text=String}.

render_both(Val, Max) ->
    [render_percent(Val, Max), " (", render_ratio(Val, Max), ")"].

render_percent(Val, Max) ->
    Percent = Val * 100 div Max,
    [wf:to_list(Percent),"%"].

render_ratio(Val, Max) ->
    [wf:to_list(Val),"/",wf:to_list(Max)].
