% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_google_chart).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, google_chart).

%% NOTE: The original implementation of this is no longer supported by Google.
%% The entirety of that image-based API was scrapped and replaced with a
%% JS-=based version that uses JSON.  Some of the older options don't seem to
%% exist in the new API.  If there's anything missing that you need, please
%% submit a PR.  All current options are supported via the use of a new
%% `options` attribute in both #google_chart and #chart_data fields.
%%
%% Some known examples:
%%
%% #google_chart.bar_space = not supported
%% #google_chart.bar_group_space = not supported
%% #google_chart.grid_x and .grid_y = I'm not sure what this actually did.
%%
%% #chart_data.min_value and .max_value = Not sure what these did exactly,
%% since there could be multiple #chart_data records in a single #google_chart
%% record.

-spec render_element(#google_chart{}) -> body().
render_element(Record) -> 
    
    Type = Record#google_chart.type,
    
    StackedTypes = [stacked_horizontal_bar, stacked_vertical_bar],
    OptChartType = case lists:member(Type, StackedTypes) of
        true -> [{isStacked, true}];
        false -> []
    end,

    OptTitle = case Record#google_chart.title of
        Title when ?WF_BLANK(Title) -> [];
        Title -> [{title, wf:to_unicode_binary(Title)}]
    end,
    
    StyleSize = format_size(Record#google_chart.height, Record#google_chart.width),

    %% TODO: These 4 options still need to be re-implemented
    %% Grid...
    %Grid = wf:f("&chg=~s,~s,~b,~b", [
    %    wf:to_list(wf:coalesce([Record#google_chart.grid_x, 0])),
    %    wf:to_list(wf:coalesce([Record#google_chart.grid_y, 0])),
    %    Record#google_chart.grid_line_length,
    %    Record#google_chart.grid_blank_length
    %]),

    % Background Colors...
    BGColor = wf:coalesce([Record#google_chart.background_color, undefined]),
    FGColor = wf:coalesce([Record#google_chart.chart_color, undefined]),
   
    Color1 = ?WF_IF(BGColor, [{fill, wf:to_binary(BGColor)}]),
    Color2 = ?WF_IF(FGColor, [{stroke, wf:to_binary(FGColor)}]),
    OptColor = case Color1 ++ Color2 of
        [] -> [];
        Colors -> [{backgroundColor, Colors}]
    end,

    OptLineColors = do_line_colors(Record#google_chart.data),
    OptLegend = [{legend, [
                    {position, Record#google_chart.legend_location}
                ]}],
    Opt3d = ?WF_IF(is_3d(Type), [{is3D, true}]),
    OptAxes = process_axes(Record#google_chart.axes),

    OptSeries = do_series(Record#google_chart.data),

    Opts = OptChartType ++ OptTitle ++ OptAxes ++ OptLegend ++ OptColor ++ OptLineColors ++ Opt3d ++ OptSeries,

    ProcessedData = process_data(Record, OptAxes, Record#google_chart.data),
    
    Tempid = wf:temp_id(),

    JS = code_from_data(Record, Tempid, Type, ProcessedData, Opts),

    wf:defer(JS),

    % Render the image tag...
    #panel{
        html_id=Record#google_chart.html_id,
        id=Tempid,
        anchor=Record#google_chart.anchor,
        class=?ADD_ELEMENT_CLASS(google_chart, [Record#google_chart.class]),
        style = [StyleSize, Record#google_chart.style],
        data_fields = Record#google_chart.data_fields,
        aria = Record#google_chart.aria
    }.

do_line_colors(Data0) ->
    Data = lists:flatten(Data0),
    Colors0 = [to_color(D#chart_data.color) || D <- Data],
    Colors = [C || C <- Colors0, not(?WF_BLANK(C))],
    ?WF_IF(Colors==[], [], [{colors, Colors}]).

do_series(Data0) ->
    Data = lists:flatten(Data0),
    NumLines = length(Data),
    %% Series are 0-indexed
    SeriesNums = lists:seq(0, NumLines-1),
    NSeries = lists:zip(SeriesNums, Data),
    SeriesOpts = lists:map(fun({N, Line}) ->
        {N, build_series_options(Line)}
    end, NSeries),
    [{series, SeriesOpts}].

build_series_options(#chart_data{
                        line_width=LineWidth,
                        line_length=LineLength,
                        blank_length=BlankLength}) ->
    LW = build_line_width(LineWidth),
    LD = build_line_dash(LineLength, BlankLength),
    LW ++ LD.

build_line_width(LW) when ?WF_BLANK(LW) ->
    [];
build_line_width(LW) ->
    [{lineWidth, wf:to_unicode_binary(LW)}].

build_line_dash(_Line, Blank) when ?WF_BLANK(Blank); Blank==0 ->
    %% if Blank is 0, then it's a solid line
    [];
build_line_dash(Line, Blank) when ?WF_BLANK(Line); Line==0 ->
    %% if "line" is 0, that's likely a mistake. Let's change it to 1.
    build_line_dash(1, Blank);
build_line_dash(Line, Blank) ->
    [{lineDashStyle, [Line, Blank]}].

to_color(X) when ?WF_BLANK(X) -> <<>>;
to_color(Str) when is_list(Str), (length(Str)==3 orelse length(Str)==6) ->
    case all_hex(Str) of
        true ->
            wf:to_binary("#" ++ Str);
        false ->
            wf:to_unicode_binary(Str)
    end;
to_color(Str) ->
    to_color(wf:to_list(Str)).

all_hex(Str) when is_list(Str), (length(Str)==3 orelse length(Str)==6) ->
    lists:all(fun(C) when ?WF_HEX(C) -> true;
                 (_) -> false
              end, Str).

format_size(Height, Width) ->
    format_size_field(height, Height) ++ format_size_field(width, Width).

format_size_field(Field, Val) when not(?WF_BLANK(Val)) ->
    %% if Val is an integer, we assume pixels. Otherwise we treat it as a string.
    wf:to_list(Field) ++ ":" ++ wf:to_list(Val) ++ ?WF_IF(is_integer(Val),"px") ++ ";";
format_size_field(_, _) -> "".

code_from_data(Rec, ID, Type, ProcessedData, Opts) ->
    JSPath = "https://www.gstatic.com/charts/loader.js",

    Operation = chart_type_operation(Type),
    OtherPackages = chart_type_packages(Type),

    JSColumns = add_columns(ProcessedData),
    JSRows = add_rows(Rec, ProcessedData),

    JSOpts = "var options = " ++ wf:json_encode(Opts) ++ ";",


    JSChart = wf:f("var chart = new google.visualization.~s(obj('~s'));", [Operation, ID]),

    JS = ["
        google.charts.load('current', {'packages':['corechart'" ++ OtherPackages ++ "]});
        google.charts.setOnLoadCallback(function() {
            var data = new google.visualization.DataTable();",
            JSColumns,"\n",
            JSRows,"\n",
            JSOpts,"\n",
            JSChart,"\n",
        "   chart.draw(data, options);
        });"
    ],

    io:format("JS: ~ts",[JS]),

    #script{
        dependency_js=JSPath,
        script=JS
    }.

add_columns(#{cols:=Cols}) ->
    [add_column(Col) || Col <- Cols].

add_column(_Col = #{type:=Type, title:=Title}) ->
    wf:f("data.addColumn('~s', '~ts');~n", [wf:js_escape(Type), wf:js_escape(Title)]).

add_rows(Rec, Data = #{num_rows:=NumRows, cols:=Cols}) ->
    NumCols = length(Cols),
    Rows = lists:seq(1, NumRows),
    [add_row(Rec, RowN, NumCols, Data) || RowN <- Rows].

add_row(Rec, RowN, NumCols, Data) ->
    %% This will look a little weird.  The first "column" for google charts is
    %% actually the X value.  So it'll always be the RowNum here.  As a result,
    %% the rest of the data will be coming from Columns 2 to NumColumns.
    Cols = lists:seq(2, NumCols),
    XValue = case is_pie(Rec#google_chart.type) of
        true ->
            get_nth_label(RowN, Rec#google_chart.axes);
        false ->
            wf:to_list(RowN)
    end,
    YValues = [format_data_for_arg(Rec, RowN, ColN, Data) || ColN <- Cols],
    case all_null(YValues) of
        true ->
            %% If all values are null, we'll skip displaying this row:
            "";
        false ->
            Values =  [XValue | YValues],
            ArgList = wf:join(Values, ","),
            "data.addRow([" ++ ArgList ++ "]);\n"
    end.

get_nth_label(N, []) ->
    "Item " ++ wf:to_list(N);
get_nth_label(N, [#chart_axis{labels=Labels}]) ->
    try "'" ++ wf:js_escape(wf:to_list(lists:nth(N, Labels))) ++ "'"
    catch _:_ -> get_nth_label(N, [])
    end.

    
format_data_for_arg(_Rec, Row, Col, Data) ->
    case ds:get(Data, {Row, Col}, undefined) of
        undefined -> "null";
        X  when is_integer(X); is_float(X) ->
            wf:to_unicode_binary(X);
        X ->
            "'" ++ wf:js_escape(wf:to_unicode_binary(X)) ++ "'"
    end.

process_axes(undefined) ->
    [];
process_axes([]) ->
    [];
process_axes(Axes) ->
    [process_axis(Axis) || Axis <- Axes].


process_axis(#chart_axis{position=Position, labels=Labels, color=Color, font_size=FontSize}) ->
    AxisKey = case lists:member(Position, [top, bottom]) of
        true -> hAxis;
        false -> vAxis
    end,
    
    Opts = [
        {title, wf:to_unicode_binary(wf:join(Labels, "/"))},
        {format, ""},
        {fontSize, wf:to_unicode_binary(FontSize)},
        {color, Color},
        {viewWindow, ""}
        %{ticks, ""}  %% ticks can correspond to the related "labels"
    ],
    {AxisKey, Opts}.

process_data(Rec, OptAxes, Data) ->
    [HAxis, _VAxis] = ds:get_list(OptAxes, [hAxis, vAxis], []),
    HATitle = ds:get(HAxis, title, "X Axis"),
    %VATitle = ds:get(VAxis, title, "Y Axis"),
    %% Cols here refers to google chart's use of columns in the data.addColumn sense, so because of that, we want to truck it to nu,ber the columns 2...LastColumn.
    ColNums = lists:seq(2, length(Data)+1),

    FirstColType = ?WF_IF(is_pie(Rec#google_chart.type), string, number),

    FirstCol = #{type=>FirstColType, title=>HATitle},
    BaseAcc = #{
        cols=>[FirstCol],
        num_rows=>0
    },

    lists:foldl(fun({ColNum, Col}, Acc) ->
        process_data_column(Rec, ColNum, Col, Acc)
    end, BaseAcc, lists:zip(ColNums, Data)).

process_data_column(_Rec, ColNum, #chart_data{legend=Legend, min_value=_MinVal, max_value=_MaxVal, values=Values}, Acc) ->
    %% need to return
    %% {Type, FieldName, Values},
    Type = ?WF_IF(all_numbers(Values), number, string),
    FieldName = Legend,
    NumVals = length(Values),
    Col = #{type=>Type, title=>FieldName},
    Acc2 = ds:update(Acc, cols, fun(Cs) -> Cs ++ [Col] end),
    Acc3 = ds:update(Acc2, num_rows, fun
                (NumRows) when NumVals > NumRows ->
                    NumVals;
                (NumRows) -> NumRows
           end),
    
    RowNums = lists:seq(1, NumVals),

    lists:foldl(fun({RowNum, V}, InnerAcc) ->
        Key = {RowNum, ColNum},
        %% we also set the RowNum record to the record num, just to ensure all are listed
        ds:set(InnerAcc, [
            {Key, V}
        ])
    end, Acc3, lists:zip(RowNums, Values)).

all_null(Vs) ->
    lists:all(fun("null") -> true;
                 (_) -> false
              end, Vs).

all_numbers(Values) ->
    lists:all(fun(V) ->
        try wf:to_float(V) of
            _ -> true
        catch
            _:_ -> false
        end
    end, Values).

chart_type_operation(line) -> "LineChart";
chart_type_operation(stacked_horizontal_bar) -> "BarChart";
chart_type_operation(stacked_vertical_bar) -> "ColumnChart";
chart_type_operation(grouped_horizontal_bar) -> "BarChart";
chart_type_operation(grouped_vertical_bar) -> "ColumnChart";
chart_type_operation(pie) -> "PieChart";
chart_type_operation(pie3d) -> "PieChart".

chart_type_packages(Type) ->
    BarTypes = [stacked_horizontal_bar, stacked_vertical_bar, grouped_horizontal_bar, grouped_vertical_bar],
    LineTypes = [line],
    IsBar = lists:member(Type, BarTypes),
    IsLine = lists:member(Type, LineTypes),

    if
        IsBar -> ",'bar'";
        IsLine -> ",'line'";
        true -> []
    end.

is_pie(Type) ->
    lists:member(Type, [pie, pie3d]).

is_3d(pie3d) -> true;
is_3d(_) -> false.
