% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Steffen Panning
% See MIT-LICENSE for licensing information.

-module (element_restful_form).
-include_lib ("wf.hrl").
-export([reflect/0, render_element/1]).

-define(IS_FORM(Tag), ( Tag == element_button         orelse
			Tag == element_checkbox       orelse
			Tag == element_hidden         orelse
			Tag == element_password       orelse
			Tag == element_radio          orelse
			Tag == element_textarea       orelse
			Tag == element_textbox        orelse
		%	Tag == element_upload         orelse
			Tag == element_restful_submit)).

reflect() -> record_info(fields, restful_form).

render_element(Record) -> 
    ?PRINT("START RENDER"),
    WithName = inject_name(Record),
    wf_tags:emit_tag(form, WithName#restful_form.body, [ 
        wf_tags:html_name(WithName#restful_form.id, 
	                  WithName#restful_form.html_name),
	{action, WithName#restful_form.action},
	{method, WithName#restful_form.method}
    ]).

%%internal
inject_name(Element) 
  when is_tuple(Element) ->
    ?PRINT("START INJECT"),
    Base = wf_utils:get_elementbase(Element),
    Module = Base#elementbase.module,
    Fields = Module:reflect(),
    New = set_html_name(Module, Fields, Element),
    case wf_utils:get_field(body, Fields, New) of
	undefined -> New;
	Body      -> NewBody = [inject_name(N) || N <- Body],
		     wf_utils:replace_field(body, NewBody, Fields, New)
    end;
inject_name(Element) ->
    Element.

set_html_name(Tag, Fields, Rec)
  when ?IS_FORM(Tag) ->
    ?PRINT("START SET_HTML_NAME"),
    {ID, Name} = {wf_utils:get_field(id, Fields, Rec),
		  wf_utils:get_field(html_name, Fields, Rec)},
    {_, NewName} = wf_tags:html_name(ID, Name),
    wf_utils:replace_field(html_name, wf_utils:normalize_id(NewName), Fields, Rec);
set_html_name(_Tag, _Fields, Rec) ->
    Rec.
