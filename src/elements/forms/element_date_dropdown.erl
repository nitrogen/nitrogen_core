%$ vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2019 Jesse Gumm
% See MIT-LICENSE for licensing information.
-module(element_date_dropdown).
-include("wf.hrl").
-export([
    reflect/0,
    transform_element/1,
    months/0,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, date_dropdown).

-spec transform_element(#date_dropdown{}) -> body().
transform_element(Record) ->
    Yid = wf:temp_id(),
    Mid = wf:temp_id(),
    Did = wf:temp_id(),
    ValTempid = wf:temp_id(),
    ValTempClass = wf:normalize_id(ValTempid),

    %% This will go on the postback element (the hidden value)
    Id = Record#date_dropdown.id,

    %% These will go on the wrapper
    Wrapperid = Record#date_dropdown.wrapperid,
    Class = Record#date_dropdown.class,
    Style = Record#date_dropdown.style,
    Actions = Record#date_dropdown.actions,

    AllowBlank = Record#date_dropdown.allow_blank,
    {Y,M,D} = to_date(AllowBlank, Record#date_dropdown.value),
    MinYear = handle_min(Record#date_dropdown.min_year),
    MaxYear = handle_max(Record#date_dropdown.max_year),
    Format = Record#date_dropdown.format,
    MonthOpts0 = month_opts(Record),
    Years = lists:seq(MaxYear, MinYear, -1),
    YearOpts0 = num_opts(Years),
    Days = lists:seq(1,31),
    DayOpts0 = num_opts(Days),
    %#span{id=Wrapperid, body=[
    Hidden = #hidden{id=Id, class=ValTempClass, text=date_to_string({Y,M,D})},

    MonthOpts = maybe_add_blank(AllowBlank, MonthOpts0),
    DayOpts = maybe_add_blank(AllowBlank, DayOpts0),
    YearOpts = maybe_add_blank(AllowBlank, YearOpts0),

    Postback = build_postback(Yid, Mid, Did, ValTempid),

    YDD = #dropdown{id=Yid, value=Y, options=YearOpts, delegate=?MODULE, postback=Postback},
    MDD = #dropdown{id=Mid, value=M, options=MonthOpts, delegate=?MODULE, postback=Postback},
    DDD = #dropdown{id=Did, value=D, options=DayOpts, delegate=?MODULE, postback=Postback},

    #panel{id=Wrapperid, class=Class, style=Style, actions=Actions, body=[
        build_format(Format, YDD, MDD, DDD),
        Hidden
    ]}.

maybe_add_blank(false, Opts) ->
    Opts;
maybe_add_blank(true, Opts) ->
    [{"", ""} | Opts].

build_postback(Yid, Mid, Did, ValTempid) ->
    {update, Yid, Mid, Did, ValTempid}.

build_format(ymd, Y, M, D) ->
    [Y,M,D];
build_format(dmy, Y, M, D) ->
    [D,M,Y];
build_format(mdy, Y, M, D) ->
    [M,D,Y];
build_format(iso, Y, M, D) ->
    build_format(ymd, Y, M, D);
build_format(usa, Y, M, D) ->
    build_format(mdy, Y, M, D).

blank_or_int(X) ->
    try wf:to_integer(X)
    catch _:_ -> ""
    end.

event({update, Yid, Mid, Did, ValTempid}) ->
    [Y0,M0,D0] = wf:mq([Yid, Mid, Did]),
    Y = blank_or_int(Y0),
    M = blank_or_int(M0),
    D = blank_or_int(D0),
    NumDays = try calendar:last_day_of_the_month(Y, M)
              catch _:_ -> 31
              end,
    ChoppingBlock = [29,30,31],
    lists:foreach(fun(Day) ->
        wf:wire(#remove_option{target=Did, value=Day}),
        ?WF_IF(Day =< NumDays, wf:wire(#add_option{target=Did, option={Day, Day}}))
    end, ChoppingBlock),
    IsValid = try calendar:valid_date({Y,M,D})
              catch _:_ -> false
              end,
    case IsValid of
        true -> 
            wf:set(Did, D),
            wf:set(ValTempid, date_to_string({Y,M,D}));
        false ->
            wf:set(ValTempid, "")
    end.

date_to_string({"", "", ""}) ->
    "";
date_to_string({Y,M,D}) ->
    wf:to_list(Y) ++ "-" ++ format_m_or_d(M) ++ "-" ++ format_m_or_d(D).

format_m_or_d(X) when X =< 9 ->
    "0" ++ wf:to_list(X);
format_m_or_d(X) ->
    wf:to_list(X).
    
handle_min(undefined) ->
    {Y, _, _} = erlang:date(),
    Y - 100;
handle_min(Y) when is_integer(Y) ->
    Y.

handle_max(undefined) ->
    {Y, _, _} = erlang:date(),
    Y + 5;
handle_max(Y) ->
    Y.

to_date(true=_AllowBlank, Blank) when Blank==undefined; Blank=="" ->
    {"", "", ""};
to_date(false=_AllowBlank, Blank) when Blank==undefined; Blank=="" ->
    erlang:date();
to_date(_, {Y,M,D}) ->
    {Y,M,D};
to_date(_, {{Y,M,D},{_,_,_}}) ->
    {Y,M,D};
to_date(AllowBlank, RawVal) ->
    %% We're going to be really forgiving of bad date values, so we trap it in
    %% a try/catch block
    try qdate:to_date(RawVal) of
        {Date, _Time} -> Date
    catch _:_ ->
        case AllowBlank of
            true -> {"", "", ""};
            false -> erlang:date()
        end
    end.

num_opts(Nums) ->
    [{X,X} || X <- Nums].

month_opts(#date_dropdown{month_names=false}) ->
    num_opts(lists:seq(1,12));
month_opts(#date_dropdown{month_fun=Fun}) ->
    Months = call_month_fun(Fun),
    lists:zip(lists:seq(1,12), Months).


call_month_fun(MonthFun) when is_function(MonthFun, 0) ->
    MonthFun;
call_month_fun({Mod, Fun}) ->
    Mod:Fun().

months() ->
    ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"].
