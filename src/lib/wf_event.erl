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
    generate_postback_script/8,
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

generate_postback_script(undefined, _Vessel, _Anchor, _ValidationGroup, _HandleInvalid, _OnInvalid, _Delegate, _ExtraParam) -> [];
generate_postback_script(Postback, Vessel, Anchor, ValidationGroup, HandleInvalid, OnInvalid, Delegate, ExtraParam) ->
    PickledPostbackInfo = serialize_event_context(Postback, Anchor, ValidationGroup, HandleInvalid, Delegate),
    VesselSelector = make_vessel_selector(Vessel),
    OnInvalidScript = case OnInvalid of
        undefined -> "null";
        _         -> ["function(){", OnInvalid, "}"]
    end,
    [
        wf:f("Nitrogen.$queue_event(~s, '~s', ", [VesselSelector, ValidationGroup]),
        OnInvalidScript,
        wf:f(", '~s', ~s);", [PickledPostbackInfo, ExtraParam])
    ].


make_vessel_selector(Vessel) when ?WF_BLANK(Vessel) ->
    "null";
make_vessel_selector(Vessel) when is_atom(Vessel) ->
    ["'",wf:normalize_id(Vessel),"'"];
make_vessel_selector(Vessel) when is_binary(Vessel); ?IS_STRING(Vessel) ->
    Vessels = string:split(Vessel, ",", all),
    make_multi_vessel_selector(Vessels);
make_vessel_selector(Vessels) when is_list(Vessels) ->
    make_multi_vessel_selector(Vessels).

make_single_vessel_selector(Vessel) when is_atom(Vessel) ->
    wf:normalize_id(Vessel);
make_single_vessel_selector(Vessel) ->
    %% NOTE: Perhaps this should check if the string is "atom-safe"
    %% (alphanumeric and underscores) and if so, treat it as an id, rather than
    %% a raw selector.  For example: should "input" should be converted to
    %% ".wfid_input" (treated as an atom/id) or should it stay "input" and be
    %% treated as a CSS selector?  For now, I'm going to say if the term is an
    %% atom, then it's an ID. If it's anything else, it's a CSS selector.
    Vessel.

make_multi_vessel_selector(Vessels) ->
    FormattedVessels = [make_single_vessel_selector(X) || X <- Vessels, not(?WF_BLANK(X))],
    JoinedVessels = wf:join(FormattedVessels, ","),
    wf:f("'~s'", [wf:js_escape(JoinedVessels)]).

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
