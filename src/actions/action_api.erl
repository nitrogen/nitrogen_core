% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_api).
-include("wf.hrl").
-export([
    render_action/1,
    event/1
]).

render_action(Record) ->
    Anchor = Record#api.anchor,
    Name = Record#api.name,
    Tag = {api_event, Record},
    [
        wf:f("obj('~s').~s = function() {", [Anchor, Name]),
        "var s = Nitrogen.$encode_arguments_object(arguments);",
        #event { postback=Tag, delegate=?MODULE, extra_param="s" },
        "};"
    ].

event({api_event, Record}) ->
    Module = wf:coalesce([Record#api.delegate, wf_context:page_module()]),
    Args = wf:q(args),
    Term = decode_args(Args),
    Module:api_event(Record#api.name, Record#api.tag, Term).

%% jquery.param wants to treat the encoding as a UTF8 string and doesn't
%% properly encode the binary data ofa Uint8Array, so we just encode to base64
%% and play it safe.
decode_args(Args) ->
    binary_to_term(base64:decode(Args), [safe]).

            
