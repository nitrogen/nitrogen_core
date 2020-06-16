% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_mermaid).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% @doc
%% Usage: #mermaid{ code="graph TB\na-->b" }
%% https://github.com/mermaid-js/mermaid

-spec reflect() -> [atom()].
reflect() -> record_info(fields, mermaid).

-spec render_element(#mermaid{}) -> body().
render_element(Record) ->
    GraphID = wf:temp_id(),
    Code = Record#mermaid.code,

    OptionsJs = options_to_js(Record#mermaid.options, Record#mermaid.diagram_options),

    Script0 = wf:f(<<"mermaid.mermaidAPI.initialize(~s);">>, [OptionsJs]),
    
    wf:wire(#script{
        dependency_js = <<"/nitrogen/mermaid.min.js">>,
        script=Script0
    }),

    Script1 = wf:f("Nitrogen.$mermaid('~s', '~s');", [GraphID, wf:js_escape(Code)]),

    wf:wire(#script{
        dependency_js = <<"/nitrogen/mermaid.min.js">>,
        script=Script1
    }),

    Classid = wf_render_elements:normalize_id(GraphID),

    Panel = #panel {
        html_id = Record#mermaid.html_id,
        id = GraphID,
        anchor = Record#mermaid.anchor,
        class = [Classid, wf_mermaid, Record#mermaid.class],
        title = Record#mermaid.title,
        body = [],
        data_fields=Record#mermaid.data_fields,
        style = Record#mermaid.style
    },
    element_panel:render_element(Panel).


options_to_js(Global, Diagram) ->
    OptionsJs = global_options_to_js(Global),
    DiagramOptionsJs = diagram_options_to_js(Diagram),
    L = lists:filter(fun check_empty/1, [OptionsJs, DiagramOptionsJs]),
    Options1 = wf:join(L, ","),
    wf:f(<<"{ ~s }">>, [Options1]).


%% Options is a list of {Key,Value} tuples
global_options_to_js([]) ->
    wf:f(<<"">>);
global_options_to_js(Options) ->
    Options1 = [format_option(X) || X <- Options],
    Options2 = wf:join(Options1, ","),
    wf:f(<<"~s">>, [Options2]).


diagram_options_to_js(undefined)->
    wf:f(<<"">>);
diagram_options_to_js({Diagram, Options})->
    OptionsJs = global_options_to_js(Options),
    wf:f(<<"~s: { ~s }">>, [Diagram, OptionsJs]).

format_option({Key, Value}) when is_atom(Value) ->
    format_option({Key, wf:to_list(Value)});
format_option({Key, Value}) when is_binary(Value) ->
    format_option({Key, wf:to_list(Value)});
format_option({Key, Value}) when ?IS_STRING(Value) ->
    wf:f(<<"~s: '~s'">>, [Key, wf:js_escape(Value)]);
format_option({Key, Value}) when Value=:=true; Value=:=false ->
    wf:f(<<"~s: ~s">>, [Key, Value]);
format_option({Key, Value}) ->
    wf:f(<<"~s: ~p">>, [Key, Value]).

check_empty(<<"">>) -> false;
check_empty(_) -> true.
