% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(modal_handler).
-include("wf.hrl").
-export ([
    process_open_action/1,
    render_open_action/1,
    process_close_action/1,
    render_close_action/1,
    open/1,
    close/1
]).

%% using "process" instead of "render" since "render" implies the return value is something that be pumped through `wf:wire`
-callback init(         handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback finish(       handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback process_open_action(#modal{},
                        handler_config(),
                        handler_state()) -> {ok, id(), script()}.
-callback process_close_action(#close_modal{},
                        handler_config(),
                        handler_state()) -> {ok, id(), script()}.

-spec process_open_action(#modal{}) -> {ok, id(), script()}.
process_open_action(Rec = #modal{}) ->
    wf_handler:call_readonly(modal_handler, process_open_action, [Rec]).

-spec process_close_action(#close_modal{}) -> {ok, id(), script()}.
process_close_action(Rec = #close_modal{}) ->
    wf_handler:call_readonly(modal_handler, process_close_action, [Rec]).


-spec render_open_action(#modal{}) -> script().
render_open_action(Rec = #modal{}) ->
    {ok, _ID, Action} = process_open_action(Rec),
    Action.

-spec open(#modal{}) -> id().
open(Rec = #modal{}) ->
    {ok, ID, Action} = process_open_action(Rec),
    wf:wire(Action),
    ID.


-spec render_close_action(#close_modal{}) -> script().
render_close_action(Rec = #close_modal{}) ->
    {ok, _ID, Action} = process_close_action(Rec),
    Action.

-spec close(#close_modal{}) -> id().
close(Rec = #close_modal{}) ->
    {ok, ID, Action} = process_close_action(Rec),
    wf:wire(Action),
    ID.


%open(Body) ->
%
%open(Body, CloseButton) ->
%
%
