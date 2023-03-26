% vim: sw=4 ts=4 et ft=erlang
-module (element_fieldset).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, fieldset).

-spec render_element(#fieldset{}) -> body().
render_element(R = #fieldset{}) ->
	LegendBody = [
		wf:html_encode(R#fieldset.legend_text,R#fieldset.legend_html_encode),
		R#fieldset.legend_body
	],
	Body = [
		wf_tags:emit_tag(legend,LegendBody, [
			{class, ["legend"]}
		]),
		wf:html_encode(R#fieldset.text,R#fieldset.html_encode),
		R#fieldset.body
	],

	wf_tags:emit_tag(fieldset,Body, [
		{class, ?ADD_ELEMENT_CLASS(fieldset, R#fieldset.class)},
		{title, R#fieldset.title},
		{style, R#fieldset.style},
        {data_fields, R#fieldset.data_fields}
	]).
