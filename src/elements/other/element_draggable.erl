% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_draggable).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, draggable).

-spec render_element(#draggable{}) -> body().
render_element(Record) -> 
    % Get properties...
    Anchor = Record#draggable.anchor,
    PickledTag = wf:pickle(Record#draggable.tag),   
    GroupClasses = groups_to_classes(Record#draggable.group),

    Handle = format_handle_opt(Record#draggable.handle),
    Helper = ?WF_IF(Record#draggable.clone, clone, original),
    Revert = json_list_to_binary(Record#draggable.revert),
    Container = json_list_to_binary(Record#draggable.container),
    Distance = json_list_to_binary(Record#draggable.distance),
    OtherOptions = [{json_list_to_binary(K), json_list_to_binary(V)} || {K,V} <- Record#draggable.options], 

    Options = Handle ++ [
        {helper, Helper},
        {revert, Revert},
        {distance, Distance},
        {scroll, json_list_to_binary(Record#draggable.scroll)},
        {containment, Container},
        {appendTo, body},
        {zIndex, json_list_to_binary(Record#draggable.zindex)}
        | OtherOptions
    ],

    JsonOptions = nitro_mochijson2:encode({struct, Options}),
     
    % Write out the script to make this element draggable...
    Script = wf:f(<<"Nitrogen.$draggable('~s', ~s, '~s');">>,
                  [Anchor, JsonOptions, PickledTag ]),
    wf:wire(Script),

    % Render as a panel...
    #panel {
        id=Record#draggable.id,
        anchor=Anchor,
        class=[draggable, GroupClasses, Record#draggable.class],
        style=Record#draggable.style,
        data_fields=Record#draggable.data_fields,
        aria=Record#draggable.aria,
        body=Record#draggable.body
    }.

format_handle_opt(undefined) ->
    [];
format_handle_opt(Handle) ->
    [{handle, wf:f(<<".~s">>, [Handle])}].

%% Mochijson encodes lists to actual lists of integers, this encodes lists to binaries
json_list_to_binary(L) when is_list(L) ->
    list_to_binary(L);
json_list_to_binary(O) ->
    O.

groups_to_classes([]) -> "";
groups_to_classes(undefined) -> "";
groups_to_classes(Groups) ->
    Groups1 = lists:flatten([Groups]),
    Groups2 = ["drag_group_" ++ wf:to_list(X) || X <- Groups1],
    string:join(Groups2, " ").

