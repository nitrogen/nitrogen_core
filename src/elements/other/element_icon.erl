% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2022 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_icon).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    scripts/0,
    scripts/2,
    css/2,
    js/2
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
    Body = body_from_prefix_icon(Prefix, Icon),
    Attributes = [
        {id, Record#icon.html_id},
        {class, [Record#icon.class, Class, SizeClass, IconClass]},
        {style, [Style, Record#icon.style]},
        {data_fields, Record#icon.data_fields},
        {aria, Record#icon.aria},
        {title, Record#icon.title},
        {'aria-hidden', true}
    ],
    wf_tags:emit_tag(Tag, Body, Attributes).

maybe_get_config(Key, Val) when Val==undefined;
                                  Val=="";
                                  Val == <<>> ->
    wf:config(Key);
maybe_get_config(_, Val) ->
    Val.

body_from_prefix_icon(material, Icon) ->
    wf:to_binary(Icon);
body_from_prefix_icon(_, _) ->
    <<>>.

icon_class(material, _) ->
    <<>>;
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
normalized_size(X) when X==1; X=='1x'; X=='1' ->
    <<"1x">>;                                 
normalized_size(X) when X==2; X=='2x'; X=='2' ->
    <<"2x">>;                                 
normalized_size(X) when X==3; X=='3x'; X=='3' ->
    <<"3x">>;                                 
normalized_size(X) when X==4; X=='4x'; X=='4' ->
    <<"4x">>;                                 
normalized_size(X) when X==5; X=='5x'; X=='5' ->
    <<"5x">>;                                 
normalized_size(X) when X==6; X=='6x'; X=='6' ->
    <<"6x">>;                                 
normalized_size(X) when X==7; X=='7x'; X=='7' ->
    <<"7x">>;                                 
normalized_size(X) when X==8; X=='8x'; X=='8' ->
    <<"8x">>;                                 
normalized_size(X) when X==9; X=='9x'; X=='9' ->
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

add_class_from_prefix(Prefix, Vsn, Type) ->
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
add_class_from_prefix_(fa, 4, _) ->
    <<"fa">>;
add_class_from_prefix_(fa, _, Type) ->
    %% If no version is provided, default to version 6 (the latest as of Aug 2022)
    add_class_from_prefix(fa, 6, Type);
%% icons8.com/line-awesome
add_class_from_prefix_(la, _, _) ->
    <<"la">>;
%% https://fonts.google.com/icons
add_class_from_prefix_(material, _, rounded) ->
    <<"material-symbols-rounded">>;
add_class_from_prefix_(material, _, sharp) ->
    <<"material-symbols-sharp">>;
add_class_from_prefix_(material, _, _) ->
    <<"material-symbols-outlined">>;
%% https://icons.getbootstrap.com
add_class_from_prefix_(bi, _, _) ->
    <<"bi">>;
add_class_from_prefix_(_, _, _) ->
    "".

size_to_style(fa, _) ->
    <<>>;
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
size_to_font_size(_) -> <<>>.

tag_from_prefix(fa) ->
    i;
tag_from_prefix(la) ->
    i;
tag_from_prefix(bi) ->
    i;
tag_from_prefix(_) ->
    span.

scripts() ->
    Prefix = wf:config(default_icon_prefix),
    Vsn = wf:config(default_icon_version),
    scripts(Prefix, Vsn).

scripts(Prefix, Vsn) ->
    [
        css(Prefix, Vsn),
        js(Prefix, Vsn)
    ].


css(Prefix, Vsn) ->
    {Url, Sha} = case wf:config(default_icon_css) of
        undefined -> css_src(Prefix, Vsn);
        Css ->
            {wf:to_binary(Css), wf:config(default_icon_css_sha)}
    end,
    render_css(Url, Sha).

render_css(X, _) when X == undefined;
                      X == "";
                      X == <<>> ->
    "";
render_css(Url, Sha) ->
    Integrity = integrity_attr(Sha),
    <<"<link rel=\"stylesheet\" href=\"",Url/binary,"\" ",Integrity/binary, " crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\" />\n">>.

js(Prefix, Vsn) ->
    {Url, Sha} = case wf:config(default_icon_js) of
        undefined -> js_src(Prefix, Vsn);
        Js ->
            {wf:to_binary(Js), wf:config(default_icon_js_sha)}
    end,
    render_js(Url, Sha).

render_js(X, _) when X == undefined;
                      X == "";
                      X == <<>> ->
    "";
render_js(Url, Sha) ->
    Integrity = integrity_attr(Sha),
    <<"<script src=\"",Url/binary,"\" ",Integrity/binary," crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\"></script>\n">>.

integrity_attr(Sha) when is_list(Sha) ->
    integrity_attr(wf:to_binary(Sha, <<>>));
integrity_attr(Sha) when is_binary(Sha), Sha =/= <<>> ->
    <<" integrity=\"",Sha/binary,"\" ">>;
integrity_attr(_) ->
    <<>>.

css_src(fa, X) when ?WF_BLANK(X) ->
    %% FA version 6 is the current newest
    css_src(fa, 6);
css_src(fa, 4) ->
    {
        <<"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">>,
        <<"sha512-SfTiTlX6kk+qitfevl/7LibUOeJWlt9rbyDn92a1DqWOw9vWG2MFoays0sgObmWazO5BQPiFucnnEAjpAB+/Sw==">>
    };
css_src(fa, 5) ->
    {
        <<"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css">>,
        <<"sha512-1ycn6IcaQQ40/MKBW2W4Rhis/DbILU74C1vSrLJxCq57o941Ym01SwNsOMqvEBFlcgUa6xLiPY/NS5R+E6ztJQ==">>
    };
css_src(fa, 6) ->
    {
        <<"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css">>,
        <<"sha512-DTOQO9RWCH3ppGqcWaEA1BIZOC6xxalwEsw9c2QQeAIftl+Vegovlnee1c9QX4TctnWMn13TZye+giMm8e2LwA==">>
    };
css_src(material, _) ->
    {
        <<"https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200">>,
        <<>>
    };
css_src(bi, _) ->
    {
        <<"https://cdnjs.cloudflare.com/ajax/libs/bootstrap-icons/1.9.1/font/bootstrap-icons.min.css">>,
        <<"sha512-5PV92qsds/16vyYIJo3T/As4m2d8b6oWYfoqV+vtizRB6KhF1F9kYzWzQmsO6T3z3QG2Xdhrx7FQ+5R1LiQdUA==">>
    };
css_src(la, _) ->
    {
        <<"https://cdnjs.cloudflare.com/ajax/libs/line-awesome/1.3.0/line-awesome/css/line-awesome.min.css">>,
        <<"sha512-vebUliqxrVkBy3gucMhClmyQP9On/HAWQdKDXRaAlb/FKuTbxkjPKUyqVOxAcGwFDka79eTF+YXwfke1h3/wfg==">>
    };
css_src(_, _) ->
     {<<>>, <<>>}.

 
js_src(fa, 5) ->
    {
        <<"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/js/all.min.js">>,
        <<"sha512-Tn2m0TIpgVyTzzvmxLNuqbSJH3JP8jm+Cy3hvHrW7ndTDcJ1w5mBiksqDBb8GpE2ksktFvDB/ykZ0mDpsZj20w==">>
    };
js_src(fa, 6) ->
    {
        <<"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/js/all.min.js">>,
        <<"sha512-8pHNiqTlsrRjVD4A/3va++W1sMbUHwWxxRPWNyVlql3T+Hgfd81Qc6FC5WMXDC+tSauxxzp1tgiAvSKFu1qIlA==">>
    };
js_src(_, _) ->
     {<<>>, <<>>}.
 
