% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_qr).
-include("wf.hrl").
-export([
	reflect/0,
	transform_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, flash).

-spec transform_element(#qr{}) -> body().
transform_element(#qr{data=Empty} = QR) when Empty =:= undefined;
                                          Empty =:= <<"">>;
                                          Empty =:= "" ->
    transform_element(QR#qr{data=wf:url()});
transform_element(#qr{data=Data, size=Size, class=Class, id=Id, title=Title}) ->
    BSize = wf:to_binary(Size),
    Cht = <<"qr">>,
    Chs = <<BSize/binary,"x",BSize/binary>>,
    Choe = "UTF-8",

    %% Not sure what this is exactly, but it's recommended to stay at H. Feel
    %% free to examine if you wish. From
    %% http://www.webmaster-source.com/2010/10/11/generate-qr-codes-on-the-fly-with-the-google-chart-api/
    Chld = "H",
    Chl = Data,

    Path = "//chart.apis.google.com/chart?",
    QS = wf:to_qs([
        {cht, Cht},
        {chs, Chs},
        {choe, Choe},
        {chld, Chld},
        {chl, Chl}
    ]),
    #image{
       id=Id,
       class=Class,
       title=Title,
       image=[Path,QS]
    }.
