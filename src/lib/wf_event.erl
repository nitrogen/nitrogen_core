% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_event).
-include("wf.hrl").
-export ([
    update_context_with_websocket_event/1,
    update_context_with_event/0,
    update_context_with_event/1,
    generate_postback_script/7,
    generate_system_postback_script/5,
    serialize_event_context/5
]).

% This module looks at the incoming request for 'eventContext' and 'pageContext' params. 
% If found, then it updates the current context, putting values into event_context
% and page_context, respectively.
%
% If not found, then it creates #event_context and #page_context records with
% values for a first request.

update_context_with_event() ->
    update_context_with_event(wf:q(eventContext)).

update_context_with_websocket_event([]) ->
    wf_context:type(postback_websocket),
    ok;
update_context_with_websocket_event(Data) ->
    {_, SerializedEvent} = lists:keyfind(<<"eventContext">>, 1, Data),
    update_context_with_event(SerializedEvent),
    postback_request = wf_context:type(),
    wf_context:type(postback_websocket),
    ok.

update_context_with_event(SerializedEvent) ->
    Event = wf_pickle:depickle(SerializedEvent),
    % Update the Context...
    PageModule = wf_context:page_module(),
    IsPostback = is_record(Event, event_context),
    case {PageModule, IsPostback} of
        {static_file, _} -> update_context_for_static_file();
        {_, false}       -> update_context_for_first_request();
        {_, true}        -> update_context_for_postback_request(Event)
    end.

update_context_for_static_file() ->
    wf_context:type(static_file),
    ok.

update_context_for_first_request() ->
    Module = wf_context:page_module(),
    wf_context:event_module(Module),
    wf_context:type(first_request),
    wf_context:anchor("page"),
    ok.

update_context_for_postback_request(Event) ->
    Anchor = Event#event_context.anchor,
    ValidationGroup = Event#event_context.validation_group,
    HandleInvalid = Event#event_context.handle_invalid,
    wf_context:type(postback_request),
    wf_context:event_context(Event),
    wf_context:anchor(Anchor),
    wf_context:event_validation_group(ValidationGroup),
    wf_context:event_handle_invalid(HandleInvalid),
    ok.

generate_postback_script(undefined, _Anchor, _ValidationGroup, _HandleInvalid, _OnInvalid, _Delegate, _ExtraParam) -> [];
generate_postback_script(Postback, Anchor, ValidationGroup, HandleInvalid, OnInvalid, Delegate, ExtraParam) ->
    PickledPostbackInfo = serialize_event_context(Postback, Anchor, ValidationGroup, HandleInvalid, Delegate),
    OnInvalidScript = case OnInvalid of
        undefined -> "null";
        _         -> ["function(){", OnInvalid, "}"]
    end,
    [
        wf:f("Nitrogen.$queue_event('~s', ", [ValidationGroup]),
        OnInvalidScript,
        wf:f(", '~s', ~s);", [PickledPostbackInfo, ExtraParam])
    ].

generate_system_postback_script(undefined, _Anchor, _ValidationGroup, _HandleInvalid, _Delegate) -> [];
generate_system_postback_script(Postback, Anchor, ValidationGroup, HandleInvalid, Delegate) ->
    PickledPostbackInfo = serialize_event_context(Postback, Anchor, ValidationGroup, HandleInvalid, Delegate),
    wf:f("Nitrogen.$queue_system_event('~s');", [PickledPostbackInfo]).

serialize_event_context(Tag, Anchor, ValidationGroup, HandleInvalid, Delegate) ->
    PageModule = wf_context:page_module(),
    EventModule = wf:coalesce([Delegate, PageModule]),
    Event = #event_context {
        module = EventModule,
        tag = Tag,
        handle_invalid = HandleInvalid,
        anchor = Anchor,
        validation_group = ValidationGroup
    },
    wf_pickle:pickle(Event).
