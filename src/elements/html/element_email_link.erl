% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_email_link).
-include("wf.hrl").
-export([
    reflect/0,
    transform_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, email_link).

-spec transform_element(#email_link{}) -> nitrogen_element().
transform_element(Rec = #email_link{}) -> 
    Link = wf_utils:copy_fields(Rec, #link{}),

    Email = Rec#email_link.email,
    Text = if
        Rec#email_link.text == [] andalso Rec#email_link.body == [] ->
            wf:html_encode(Email);
        true -> Rec#email_link.text
    end,

    Link#link{
        url=[<<"mailto:">>,Email],
        new=false,
        text=Text
    }.
