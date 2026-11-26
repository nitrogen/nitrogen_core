%% vim: ft=nitrogen
-module(element_calendar).
-include("wf.hrl").

-export([
    reflect/0,
    transform_element/1,
    event/1,
    drop_event/2
]).

-record(drop_tag, {calendar_tag, delegate, date}).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, calendar).

-spec transform_element(#calendar{}) -> body().
transform_element(_Rec = #calendar{id=Id0, delegate=Delegate0, actions=Actions, aria=Aria,
        class=Class, style=Style, tag=CalTag, month=M, year=Y, html_id=Htmlid, data_fields=Data}) ->
    Delegate = wf:coalesce([Delegate0, wf:page_module()]),
    FirstDay = {{Y, M, 1}, {0,0,0}},
    %% Start Week on Sunday
    Start = qdate:beginning_week(7, FirstDay),
    %% end at the end of the week for the last week of the month (so end on Saturday)
    End = qdate:end_week(7, qdate:end_month(FirstDay)),
    Id = ?WF_IF(Id0==undefined, wf:temp_id(), Id0),
    DateTimes = [qdate:to_date(D) || D <- qdate:range_days(1, Start, End)],
    Days = [Date || {Date, _Time} <- DateTimes],
    Items = load_items(Delegate, CalTag, Y, M),
    {Today, _} = qdate:to_date(os:timestamp()),
    #panel{id=Id, class=[nitrogen_calendar, Class], style=Style, actions=Actions, aria=Aria, html_id=Htmlid, data_fields=Data, body=[
        #table{rows=[
            #tablerow{cells=[
                #tableheader{colspan=7, body=[
                    #h3{style="text-align:center", text=qdate:to_string("F Y", FirstDay)}
                ]}
            ]},
            #tablerow{class=nitrogen_calendar_day_header, cells=[
                #tableheader{text="Sunday"},
                #tableheader{text="Monday"},
                #tableheader{text="Tuesday"},
                #tableheader{text="Wednesday"},
                #tableheader{text="Thursday"},
                #tableheader{text="Friday"},
                #tableheader{text="Saturday"}
            ]},
            draw_weeks(Today, Delegate, CalTag, Items, Days)
        ]}
    ]}.



draw_weeks(_, _, _, _, []) ->
    [];
%% We're garuanteed that the "Days" list is a multiple of 7, because we're always getting the Sunday through Saturday, padding at the front and back as necessary
draw_weeks(Today, Delegate, CalTag, Items, [D1,D2,D3,D4,D5,D6,D7 | Rest]) ->
    [
        draw_days(Today, Delegate, CalTag, Items, [D1,D2,D3,D4,D5,D6,D7]),
        draw_weeks(Today, Delegate, CalTag, Items, Rest)
    ].

draw_days(Today, Delegate, CalTag, Items, Days) ->
    #tablerow{cells=[
        [draw_day(Today, Delegate, CalTag, Items, D) || D <- Days]
    ]}.

draw_day(Today, Delegate, CalTag, Items, Date = {_, _, D}) ->
    DayItems = get_date_items(Date, Items),
    IsInvalidDrop = has_invalid_drop(DayItems),
    Body = draw_items(DayItems),
    CellClass = ?WF_IF(Today==Date, calendar_today, ""),
    Body2 = case IsInvalidDrop of
        true -> Body;
        false ->
            DropTag = #drop_tag{
                delegate=Delegate,
                calendar_tag=CalTag,
                date=Date
            },
            #droppable{class=nitrogen_calendar_drop, delegate=?MODULE, tag=DropTag, body=Body}
    end,

    #tablecell{class=CellClass, body=[
        #panel{class=nitrogen_calendar_day_wrapper, body=[
            #panel{class=nitrogen_calendar_day, text=D},
            #panel{class=nitrogen_calendar_day_body, body=[
                Body2
            ]}
        ]}
    ]}.

has_invalid_drop(Items) ->
    lists:any(fun(#calendar_invalid_drop{}) -> true;
                 (_) -> false
              end, Items).

draw_items(Items) ->
    [draw_item(I) || I <- Items].

draw_item(I = #calendar_item{draggable=false}) ->
    #panel{class=nitrogen_calendar_static_item, body=draw_item_body(I)};
draw_item(I = #calendar_item{draggable=true, tag=ItemTag}) ->
    #draggable{tag=ItemTag, class=nitrogen_calendar_draggable_item, body=draw_item_body(I)};
draw_item(_) ->
    [].

draw_item_body(I) ->
    #panel{
        class=[I#calendar_item.class, nitrogen_calendar_item_body],
        body=I#calendar_item.body,
        text=I#calendar_item.text
    }.


get_date_items(_, undefined) -> [];
get_date_items(Day, Items) ->
    case dict:find(Day, Items) of
        {ok, Is} -> Is;
        error -> []
    end.

load_items(Delegate, Tag, Y, M) ->
    case erlang:function_exported(Delegate, calendar_load_event, 3) of
        true ->
            RawItems = Delegate:calendar_load_event(Tag, Y, M),
            lists:foldl(fun(Item, Acc) ->
                Date = extract_item_date(Item),
                dict:append(Date, Item, Acc)
            end, dict:new(), RawItems);
        false ->
            undefined        
    end.

extract_item_date(#calendar_item{date=Date}) ->
    Date;
extract_item_date(#calendar_invalid_drop{date=Date}) ->
    Date.

drop_event(ItemTag, #drop_tag{calendar_tag=CalTag, delegate=Delegate, date=Date}) ->
    ok.

event(_) ->
    ok.
