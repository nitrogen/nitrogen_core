% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_icon).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, icon).

-spec render_element(#icon{}) -> body().
render_element(Record = #icon{icon=Icon, prefix=Prefix0, version=Vsn0, type=Type, size=Size}) ->
    Prefix = maybe_get_config(default_icon_prefix, Prefix0),
    Vsn = maybe_get_config(default_icon_version, Vsn0),
    Class = add_class_from_prefix(Prefix, Vsn, Type),
    SizeClass = size_class(Prefix, Size),
    Tag = tag_from_prefix(Prefix),
    Style = size_to_style(Prefix, Size),
    IconClass = icon_class(Prefix, Icon),
    Attributes = [
        {id, Record#icon.html_id},
        {class, [Record#icon.class, Class, SizeClass, IconClass]},
        {style, [Style, Record#icon.style]},
        {data_fields, Record#icon.data_fields},
        {'aria-hidden', true}
    ],
    wf_tags:emit_tag(Tag, [], Attributes).

maybe_get_config(Key, Val) when Val=/=undefined,
                                  Val=/="",
                                  Val =/= <<>> ->
    wf:config(Key);
maybe_get_config(_, Val) ->
    Val.

icon_class(Prefix0, Icon0) ->
    Prefix = wf:to_binary(Prefix0),
    Icon = wf:to_binary(Icon0),
    <<Prefix/binary,"-",Icon/binary>>.

normalized_size(X) when X=='2xs'; X=='xxs'; X=='xxsmall' ->
    <<"2xs">>;
normalized_size(X) when X=='xsmall'; X=='xs' ->
    <<"xs">>;
normalized_size(X) when X=='sm'; X=='small' ->
    <<"sm">>;
normalized_size(X) when X==large; X==lg ->
    <<"lg">>;
normalized_size(X) when X=='xl'; X=='xlarge' ->
    <<"xl">>;
normalized_size(X) when X=='xxl'; X=='2xl'; X=='xxlarge' ->
    <<"2xl">>;
normalized_size(X) when X==1; X=='1x' ->
    <<"1x">>;
normalized_size(X) when X==2; X=='2x' ->
    <<"2x">>;
normalized_size(X) when X==3; X=='3x' ->
    <<"3x">>;
normalized_size(X) when X==4; X=='4x' ->
    <<"4x">>;
normalized_size(X) when X==5; X=='5x' ->
    <<"5x">>;
normalized_size(X) when X==6; X=='6x' ->
    <<"6x">>;
normalized_size(X) when X==7; X=='7x' ->
    <<"7x">>;
normalized_size(X) when X==8; X=='8x' ->
    <<"8x">>;
normalized_size(X) when X==9; X=='9x' ->
    <<"9x">>;
normalized_size(X) ->
    wf:to_binary(X, <<>>).

%% fontawesome
size_class(Prefix0, Size0) ->
    case {wf:to_binary(Prefix0),normalized_size(Size0)} of
        {<<>>, _} -> <<>>;
        {_, <<>>} -> <<>>;
        {Prefix, Size} -> <<Prefix/binary,"-",Size/binary>>
    end.

add_class_from_prefix(Prefix, Vsn0, Type) ->
    Vsn = wf:to_integer(Vsn0, undefined),
    add_class_from_prefix_(wf:to_atom(Prefix, icon), wf:to_integer(Vsn, 0), wf:to_existing_atom(Type, regular)).

%% fontawesome things
add_class_from_prefix_(fa, 5, solid) ->
    <<"fas">>;
add_class_from_prefix_(fa, 5, light) ->
    <<"fal">>;
add_class_from_prefix_(fa, 5, thin) ->
    <<"fal">>;
add_class_from_prefix_(fa, 5, duotone) ->
    <<"fad">>;
add_class_from_prefix_(fa, 5, _) ->
    <<"far">>;
add_class_from_prefix_(fa, 6, solid) ->
    <<"fa-solid">>;
add_class_from_prefix_(fa, 6, light) ->
    <<"fa-light">>;
add_class_from_prefix_(fa, 6, thin) ->
    <<"fa-thin">>;
add_class_from_prefix_(fa, 6, duotone) ->
    <<"fa-duotone">>;
add_class_from_prefix_(fa, 6, _) ->
    <<"fa-regular">>;
add_class_from_prefix_(fa, _, _) ->
    <<"fa">>;
%% icons8.com/line-awesome
add_class_from_prefix_(la, _, _) ->
    <<"la">>;
add_class_from_prefix_(_, _, _) ->
    "".

size_to_style(fa, _) ->
    "";
size_to_style(_, Size0) ->
    case size_to_font_size(normalized_size(Size0)) of
        <<>> -> <<>>;
        Fontsize -> <<"font-size:",Fontsize/binary,";">>
    end.
    
size_to_font_size(<<"2xs">>) -> <<"0.625em">>;
size_to_font_size(<<"xs">>) -> <<"0.75em">>;
size_to_font_size(<<"sm">>) -> <<"0.875em">>;
size_to_font_size(<<"lg">>) -> <<"1.25em">>;
size_to_font_size(<<"xl">>) -> <<"1.5em">>;
size_to_font_size(<<"2xl">>) -> <<"2em">>;
size_to_font_size(<<"2x">>) -> <<"2em">>;
size_to_font_size(<<"3x">>) -> <<"3em">>;
size_to_font_size(<<"4x">>) -> <<"4em">>;
size_to_font_size(<<"5x">>) -> <<"5em">>;
size_to_font_size(<<"6x">>) -> <<"6em">>;
size_to_font_size(<<"7x">>) -> <<"7em">>;
size_to_font_size(<<"8x">>) -> <<"8em">>;
size_to_font_size(<<"9x">>) -> <<"9em">>;
size_to_font_size(_) -> "".

tag_from_prefix(fa) ->
    i;
tag_from_prefix(la) ->
    i;
tag_from_prefix(icon) ->
    span.
