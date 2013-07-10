% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_password).
-include_lib ("wf.hrl").
-export([
    reflect/0,
    transform_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, password).

-spec transform_element(Record :: nitrogen_element()) -> nitrogen_element().
transform_element(Record) -> 
    Textbox = wf_utils:copy_fields(Record, #textbox{}),
    Textbox#textbox{
        class=[password,Record#password.class],
        type=password
    }.
