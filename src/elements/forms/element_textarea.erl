% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textarea).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render_element(Record) -> 
    Text = html_encode(Record#textarea.text, Record#textarea.html_encode),
    wf_tags:emit_tag(textarea, Text, [
        {id, Record#textarea.html_id},
	{class, [textarea, Record#textarea.class]},
	{style, Record#textarea.style}
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
