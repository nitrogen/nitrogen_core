% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
%
% Copyright (c) 2013 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_mobile_panel).
-include("wf.hrl").
-export([
    reflect/0,
    transform_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, mobile_panel).

-spec transform_element(#mobile_panel{}) -> #panel{}.
transform_element(Record) -> 
    Panel = wf_utils:copy_fields(Record, #panel{}),
    Panel#panel{
        data_fields=[
            {role, panel},
            {position, Record#mobile_panel.position},
            {display, Record#mobile_panel.display_mode},
            {dismissible, Record#mobile_panel.dismissible},
            {theme, Record#mobile_panel.theme}
            | Record#mobile_panel.data_fields
        ]
    }.
