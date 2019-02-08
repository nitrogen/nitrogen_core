% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radiogroup).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, radiogroup).

-spec render_element(#radiogroup{}) -> body().
render_element(Record) -> 
    % Set the group to the current HtmlID...
    Anchor = Record#radiogroup.anchor,

    NameFun = set_name_fun(Anchor),

    {Time, Body} = timer:tc(fun() ->
        wf_render_elements:recurse_body(NameFun, Record#radiogroup.body)
    end),

    % Render the record...
    element_panel:render_element(#panel {
        id=Record#radiogroup.id,
        anchor=Record#radiogroup.anchor,
        class=[radiogroup, Record#radiogroup.class],
        title=Record#radiogroup.title,
        style=Record#radiogroup.style,
        data_fields=Record#radiogroup.data_fields,
        body=Body
    }).

set_name_fun(Name) ->
    fun
        (X = #radio{}) ->
            X#radio{name=Name};
        (X) ->
            X
    end.
