% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% Contributions from Tom McNulty (tom.mcnulty@cetiforge.com)
% See MIT-LICENSE for licensing information.

-module (wf_tags).
-author('tom.mcnulty@cetiforge.com').
-include_lib ("wf.hrl").
-define(NO_SHORT_TAGS(TagName),(
    TagName =/= 'div' andalso 
    TagName =/= 'span' andalso 
    TagName =/= 'label' andalso 
    TagName =/= 'textarea' andalso 
    TagName =/= 'table' andalso 
    TagName =/= 'tr' andalso 
    TagName =/= 'th' andalso 
    TagName =/= 'td' andalso 
    TagName =/= 'p' andalso
    TagName =/= 'i' andalso
    TagName =/= 'a' andalso
    TagName =/= 'ul' andalso
    TagName =/= 'ol' andalso
    TagName =/= 'select' andalso
    TagName =/= 'script' andalso
    TagName =/= 'legend' andalso
    TagName =/= 'fieldset' andalso
    TagName =/= 'h1' andalso
    TagName =/= 'h2' andalso
    TagName =/= 'h3' andalso
    TagName =/= 'h4' andalso
    TagName =/= 'h5' andalso
    TagName =/= 'h6' andalso
    TagName =/= 'iframe')).

-export ([emit_tag/2, emit_tag/3, html_name/2]).

html_name(Id, Name)
  when Id =:= [] orelse
       Id =:= undefined ->
    html_name(wf_render_elements:temp_id(), Name);
html_name(Id, Name) ->
    case Name of
        undefined -> {name, Id};
        ""        -> {name, Id};
        Name      -> {name, Name}
    end.

%%%  Empty tags %%%

emit_tag(TagName, Props) ->
    BTagName = wf:to_binary(TagName),
    BProps = write_props(Props),
    <<"<",BTagName/binary,BProps/binary,"/>">>.

%%% Tags with child content %%%

% empty text and body
emit_tag(TagName, [[], []], Props) when ?NO_SHORT_TAGS(TagName) ->
    emit_tag(TagName, Props);

emit_tag(TagName, [], Props) when ?NO_SHORT_TAGS(TagName) ->
    emit_tag(TagName, Props);

emit_tag(TagName, Content, Props) ->
    BTagName = wf:to_binary(TagName),
    BProps = write_props(Props),
    [
        <<"<",BTagName/binary,BProps/binary,">">>,
        Content,
        <<"</", BTagName/binary, ">">>
    ].    

%%% Property display functions %%%

write_props([]) ->
    <<>>;
write_props([H|T]) ->
    %HBin = ?WF_PROFILE({props, H}, display_property(H)),
    HBin = display_property(H),
    TBin = write_props(T),
    <<HBin/binary,TBin/binary>>.

display_property(undefined) ->
    <<>>;

display_property([]) ->
    <<>>;

display_property({Prop}) ->
    display_property(Prop);

display_property(Prop) when is_atom(Prop) ->
    display_property(wf:to_binary(Prop));

display_property(Prop) when is_binary(Prop) ->
    <<" ",Prop/binary>>;

display_property(Prop) when ?IS_STRING(Prop) ->
    display_property(wf:to_unicode_binary(Prop));

%% Data fields are special in HTML5.
%% In this case, the DataTags value is expected to be a
%% proplist of [{field,Value}]. Emitted will be data-field="value".
%% "data-" gets prefixed on the fieldnames.
display_property({data_fields,DataTags}) ->
    BinTags = data_tags(DataTags),
    <<" ",BinTags/binary>>;

display_property({aria, AriaTags}) ->
    BinTags = aria_tags(AriaTags),
    <<" ",BinTags/binary>>;

display_property({Prop, V}) when is_atom(Prop) ->
    display_property({wf:to_binary(Prop), V});

%% Most HTML tags don't care about a property with an empty string as its value
%% Except for the "value" tag on <option> and other form tags.
%% In this case, we emit the 'value' propery even if it's an empty value.
display_property({Prop, Value}) when Prop =/= <<"value">> andalso ?WF_BLANK(Value) ->
    <<>>;

display_property({Prop, Value}) when is_list(Prop) ->
    display_property({wf:to_binary(Prop), Value});

display_property({Prop, Value}) when is_integer(Value); is_atom(Value); is_float(Value) ->
    BinValue = wf:to_binary(Value),
    <<" ",Prop/binary,"=\"",BinValue/binary, "\"">>;

%% 'class' is a special kind of field, which will be reformatted to handle
display_property({<<"class">>, Values}) ->
    Class = format_class(Values),
    %StrValues = wf:to_string_list(Values),
    %StrValues1 = string:strip(string:join(StrValues, " ")),
    %StrValues2 = wf_utils:replace(StrValues1, ".", ""),
    %BinValues = wf:to_binary(StrValues2),
    <<" class=\"", Class/binary, "\"">>;

display_property({Prop, Value}) when is_list(Value) ->
    display_property({Prop, wf:to_unicode_binary(Value)});


display_property({Prop, Value}) when is_binary(Prop), is_binary(Value) ->
    <<" ", Prop/binary, "=\"", Value/binary, "\"">>.


format_class([]) ->
    <<>>;
format_class(<<".",Class/binary>>) ->
    Class;
format_class(Class) when is_binary(Class) ->
    Class;
format_class("." ++ Class) ->
    wf:to_binary(Class);
format_class(Class) when is_atom(Class) ->
    wf:to_binary(Class);
format_class(Class) when ?IS_STRING(Class) ->
    wf:to_binary(Class);
format_class([H|T]) ->
    HBin = format_class(H),
    TBin = format_class(T),
    <<HBin/binary," ",TBin/binary>>.


data_tags(Tags) ->
    prefix_tags(<<"data-">>, Tags).

aria_tags(Tags) ->
    prefix_tags(<<"aria-">>, Tags).

prefix_tags(_Prefix, []) ->
    <<>>;
prefix_tags(Prefix, [H|T]) ->
    HBin = display_property(prefix_tag(Prefix,H)),
    TBin = prefix_tags(Prefix,T),
    <<HBin/binary,TBin/binary>>.

prefix_tag(Prefix, {FieldName,Value}) ->
    DataField = wf:to_unicode_binary(FieldName),
    {<<Prefix/binary,DataField/binary>>,Value};
prefix_tag(Prefix, {FieldName}) ->
    DataField = wf:to_unicode_binary(FieldName),
    {<<Prefix/binary,DataField/binary>>};
prefix_tag(Prefix, FieldName) when is_atom(FieldName) ->
    prefix_tag(Prefix, {FieldName}).
