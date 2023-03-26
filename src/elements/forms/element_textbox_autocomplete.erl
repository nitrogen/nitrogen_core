% vim: sw=4 ts=4 et ft=erlang
-module (element_textbox_autocomplete).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, textbox_autocomplete).

-spec render_element(#textbox_autocomplete{}) -> body().
render_element(Record) ->
    % Get properties...
    Delegate = Record#textbox_autocomplete.delegate,
    Tag = Record#textbox_autocomplete.tag,
    Anchor = Record#textbox_autocomplete.anchor,
    AutoCompleteMinLength = Record#textbox_autocomplete.minLength,
    AutoCompleteDelay = Record#textbox_autocomplete.delay,

    % Write out the script to make this element autocompletable...
    AutoCompleteEnterPostbackInfo = wf_event:serialize_event_context({autocomplete_enter_event, Delegate, Tag}, Anchor, undefined, false, ?MODULE),
    AutoCompleteSelectPostbackInfo = wf_event:serialize_event_context({autocomplete_select_event, Delegate, Tag }, Anchor, undefined, false, ?MODULE ),

    AutoCompleteOptions = [
        {dataType, <<"json">>},
        {minLength, AutoCompleteMinLength},
        {delay, AutoCompleteDelay}
    ],

    AutoCompleteScript = #script {
        script = wf:f("Nitrogen.$autocomplete('~s', ~s, '~s', '~s');", [
          Anchor,
          wf:json_encode(AutoCompleteOptions),
          AutoCompleteEnterPostbackInfo,
          AutoCompleteSelectPostbackInfo
        ])
    },
    wf:wire(AutoCompleteScript),

    % Render as a textbox.
    Textbox = wf_utils:copy_fields(Record, #textbox{}),
    Textbox1 = Textbox#textbox{class=?ADD_ELEMENT_CLASS(textbox_autocomplete, Textbox#textbox.class)},
    element_textbox:render_element(Textbox1).

-spec event(any()) -> any().
event({autocomplete_select_event, Delegate, SelectTag})->
    RawItem = wf:q(select_item),
    SelectItem = wf:json_decode(RawItem),
    Module = wf:coalesce([Delegate, wf:page_module()]),
    Module:autocomplete_select_event(SelectItem, SelectTag);

event({autocomplete_enter_event, Delegate, EnterTag})->
    SearchTerm = wf:q(search_term),
    Module = wf:coalesce([Delegate, wf:page_module()]),
    Json = Module:autocomplete_enter_event(SearchTerm, EnterTag),
    change_headers_for_nonwebsocket(),
    wf_context:data(Json).

change_headers_for_nonwebsocket() ->
    case wf_context:type() of
        postback_websocket ->
            ok;
        _ ->
            wf_context:type(first_request),
            wf:content_type("application/json")
    end.
