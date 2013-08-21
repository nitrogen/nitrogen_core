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

    AutoCompleteOptions = {struct, [
        {dataType, <<"json">>},
        {minLength, AutoCompleteMinLength},
        {delay, AutoCompleteDelay}
    ]},

    AutoCompleteScript = #script {
        script = wf:f("Nitrogen.$autocomplete('~s', ~s, '~s', '~s');", [
          Anchor,
          nitro_mochijson2:encode(AutoCompleteOptions),
          AutoCompleteEnterPostbackInfo,
          AutoCompleteSelectPostbackInfo
        ])
    },
    wf:wire(AutoCompleteScript),

    % Render as a textbox.
    Value = wf:html_encode(Record#textbox_autocomplete.text, Record#textbox_autocomplete.html_encode),
    wf_tags:emit_tag(input, [
        {id, Record#textbox_autocomplete.html_id},
        {type, text},
        {class, [textbox_autocomplete, Record#textbox_autocomplete.class]},
        {style, Record#textbox_autocomplete.style},
        {data_fields, Record#textbox_autocomplete.data_fields},
        {value, Value}
    ]).

-spec event(any()) -> any().
event({autocomplete_select_event, Delegate, SelectTag})->
    SelectItem = nitro_mochijson2:decode(wf:q(select_item)),
    Module = wf:coalesce([Delegate, wf:page_module()]),
    Module:autocomplete_select_event(SelectItem, SelectTag);

event({autocomplete_enter_event, Delegate, EnterTag})->
    SearchTerm = wf:q(search_term),
    wf_context:type(first_request),
    wf:content_type("application/json"),
    Module = wf:coalesce([Delegate, wf:page_module()]),
    wf_context:data([
      Module:autocomplete_enter_event(SearchTerm, EnterTag)
    ]).

