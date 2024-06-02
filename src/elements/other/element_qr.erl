% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2024 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_qr).
-include("wf.hrl").
-export([
	reflect/0,
	render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, qr).

-spec render_element(#qr{}) -> body().
render_element(#qr{data=Empty} = QR) when Empty =:= undefined;
                                          Empty =:= <<"">>;
                                          Empty =:= "" ->
    render_element(QR#qr{data=wf:url()});
render_element(#qr{data=Data, size=Size, class=Class, title=Title, data_fields=DataFields, style=Style}) ->
    {SvgAttrs, Body} = nqr_svg:tag(Data),
    XMLNS = "http://www.w3.org/2000/svg",
    SvgAttrs2 = [{xmlns, XMLNS} | SvgAttrs],
    Attrs = [
        {class, Class},
        {title, Title},
        {data_fields, DataFields},
        {width, Size},
        {height, Size},
        {style, Style}
    ],
    Attrs2 = SvgAttrs2 ++ Attrs,
    wf_tags:emit_tag(svg, Body, Attrs2).
