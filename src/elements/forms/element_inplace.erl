% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module (element_inplace).
-include("wf.hrl").
-export([
    reflect/0,
    transform_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, inplace).

-spec transform_element(#inplace{}) -> body().
transform_element(#inplace{
        id=WrapperID,
        actions=Actions,
        text=Text,
        tag=Tag,
        delegate=Delegate,
        edit=Edit,
		view=View,
        class=Class,
        style=Style,
        start_mode=StartMode,
        data_fields=DataFields,
        replace_id=ReplaceID,
        replace_value=ReplaceValue,
        hover_text=HoverText0,
        aria=Aria}) ->

    [OKButtonID, CancelButtonID,
     ViewPanelID, EditPanelID,
     EditID, ViewID,
     MouseOverID] = wf:temp_ids(7),

    HoverText = wf:coalesce([HoverText0, "Click to edit"]),

    store_default_value(EditID, Text),

	% Set up the events...

	Controls = {
        ViewPanelID,
        ViewID,
        EditPanelID,
        EditID
    },

	OKPostback = {ok, Delegate, Controls, Tag},
	CancelPostback = {cancel, Controls, Tag},

    ViewActions = [
		#buttonize{target=ViewPanelID},
        #event{type=click, actions=[
            #hide{target=ViewPanelID},
            #show{target=EditPanelID},
            wf:f("let e = objs('~s'); e.focus(); e.select();", [EditID])
        ]}

    ],

	% Create the edit...

	EditAction = [
        #event{type=enterkey, shift_key=false, actions=#click{target=OKButtonID}},
		#event{type=keyup, keycode=27, actions=#click{target=CancelButtonID}}
	],

    ModifyViewFun = fun(Body) ->
        wf:defer(ViewID, [
            #event{type=mouseover, target=MouseOverID, actions=#show{}},
            #event{type=mouseout, target=MouseOverID, actions=#hide{}}
        ]),

        [
            Body,
            #span{id=MouseOverID, class=instructions, text=HoverText, style="display:none"}
        ]
    end,

    ModifyEditFun = fun(Body) ->
        [
            maybe_modify_validators(Body, OKButtonID),
            #button{id=OKButtonID, class=inplace_ok, text="OK", delegate=?MODULE, postback=OKPostback},
            #button{id=CancelButtonID, class=inplace_cancel, text="Cancel", delegate=?MODULE, postback=CancelPostback}
        ]
    end,

    EditFun = maybe_make_edit_view_fun(Edit, ReplaceID, ReplaceValue, ModifyEditFun),
    ViewFun = maybe_make_edit_view_fun(View, ReplaceID, ReplaceValue, ModifyViewFun),

    store_edit_fun(EditID, EditFun),
    store_view_fun(ViewID, ViewFun),

    wf:defer(EditID, EditAction),

	% No value in view mode cause the view panel unclickable thus
	% we set edit mode in that case.

	StartMode1 = ?WF_IF(not(?WF_BLANK(string:strip(Text))), StartMode, edit),

    {ViewStyle, EditStyle} = case StartMode1 of
        view -> {"", "display:none"};
        edit -> {"display:none", ""}
    end,

	% Create the main panel...

    #panel{id=WrapperID, class=[inplace, Class], data_fields=DataFields, aria=Aria, style=Style, actions=Actions, body=[
        #panel{
            id=ViewPanelID,
            style=ViewStyle,
            class=view,
            actions=ViewActions,
            body=ViewFun(ViewID, Text)
        },
        #panel{
           id=EditPanelID,
           style=EditStyle,
           class=edit,
           body=EditFun(EditID, Text)
        }
    ]}.

maybe_make_edit_view_fun(undefined, _ReplaceID, _ReplaceValue, PostFun)  ->
    fun(ID, Value) ->
        Body = #span{id=ID, class=label, text=Value},
        PostFun(Body)
    end;
maybe_make_edit_view_fun(Fun, ReplaceID, ReplaceValue, undefined) ->
    maybe_make_edit_view_fun_inner(Fun, ReplaceID, ReplaceValue);
maybe_make_edit_view_fun(Fun, ReplaceID, ReplaceValue, PostFun) ->
    InnerFun = maybe_make_edit_view_fun_inner(Fun, ReplaceID, ReplaceValue),
    fun(ID, Value) ->
        Body = InnerFun(ID, Value),
        PostFun(Body)
    end.

maybe_make_edit_view_fun_inner(Fun, _, _) when is_function(Fun, 2) ->
    Fun;
maybe_make_edit_view_fun_inner(Rec, ReplaceID, ReplaceValue) when ?IS_ELEMENT(Rec) orelse is_list(Rec) ->
    case has_explicit_value(Rec, [ReplaceID, ReplaceValue]) of
        false ->
            make_simple_fun(Rec);
        true ->
            make_deeper_fun(Rec, ReplaceID, ReplaceValue) 
    end;
maybe_make_edit_view_fun_inner(Rec, _ReplaceID, _ReplaceValue) ->
    logger:error("Value: ~p is not valid to be the body of #inplace.edit or #inplace.view~n", [Rec]),
    error({invalid_inplace_view_or_edit, Rec}).

make_deeper_fun(Rec, ReplaceID, ReplaceValue) ->
    PredID = fun(X) -> X==ReplaceID end,
    PredVal = fun(X) -> X==ReplaceValue end,
    fun(ID, Val) ->
        UpdateIDFun = fun(_) -> ID end,
        UpdateValFun = fun(_) -> Val end,
        Rec2 = update_deep(Rec, PredID, UpdateIDFun),
        update_deep(Rec2, PredVal, UpdateValFun)
    end.


make_simple_fun(Rec) when ?IS_ELEMENT(Rec) ->
    %Module = element(#elementbase.module, Rec),
    %Fields = Module:fields(),
    fun(ID, Val) ->
        set_id_and_value(Rec, ID, Val)
    end;
make_simple_fun(X) ->
    logger:error("If the provided #inplace.view or #inplace.edit value does not contain an id='##' and a value of '$$', the provided value must be a simple element record (not a list or anything else)."),
    error({invalid_inplace_view_or_edit, X}).

%% looking for the atom '##' for id
%% looking for the atom '$$' for value
has_explicit_value(Rec, SearchVals) ->
    Pred = fun
        (X) when ?IS_ELEMENT(X) ->
            Attrs = tl(tl(tuple_to_list(X))),
            %?PRINT({looking_for_explicit, SearchVals, Attrs}),
            wf_utils:any_member(SearchVals, Attrs);
        (_) ->
            %?PRINT({no_explicit_yet, X}),
            false
    end,
    find_first(Rec, Pred).

find_first(Rec, Pred) when ?IS_ELEMENT(Rec);
                           ?IS_ACTION(Rec) ->
                           %% ?IS_VALIDATOR(Rec) ->
    case Pred(Rec) of
        true -> true;
        {true, X} -> {ok, X};
        false ->
            Attrs = tl(tl(tuple_to_list(Rec))),
            find_first(Attrs, Pred)
    end;
find_first([H|T], Pred) ->
    case find_first(H, Pred) of
        true -> true;
        {true, X} -> X;
        false ->
            find_first(T, Pred)
    end;
find_first([], _Pred) ->
    false;
find_first(_X, _Pred) ->
    false.

update_deep(Rec, Pred, UpdateFun) when ?IS_ELEMENT(Rec) ->
    case Pred(Rec) of
        true ->
            UpdateFun(Rec);
        false ->
            [RecName, Module | Attrs] = tuple_to_list(Rec),
            Attrs2 = [update_deep(Attr, Pred, UpdateFun) || Attr <- Attrs],
            list_to_tuple([RecName, Module | Attrs2])
    end;
update_deep(List, Pred, UpdateFun) when is_list(List) ->
    [update_deep(X, Pred, UpdateFun) || X <- List];
update_deep(X, Pred, UpdateFun) ->
    case Pred(X) of
        true -> UpdateFun(X);
        false -> X
    end.

%% %% Not implmeneted yet
%% update_first_sub_element(Rec) ->
%%     Rec.


set_id_and_value(Rec, ID, Value) ->
    Rec2 = set_id(Rec, ID),
    set_value(Rec2, Value).

set_id(Rec, ID) ->
    setelement(#elementbase.id, Rec, ID).

set_value(Rec, Value) ->
    case value_field(Rec) of
        undefined ->
            error({value_field_unknown, Rec});
        Field ->
            setelement(Field, Rec, Value)
    end.

%% A few basic shortcuts, then make it look deeper
value_field(#textbox{}) -> #textbox.text;
value_field(#hidden{}) -> #hidden.text;
value_field(#textarea{}) -> #textarea.text;
value_field(#password{}) -> #password.text;
value_field(#time{}) -> #time.datetime;
value_field(#datepicker_textbox{}) -> #datepicker_textbox.text;
value_field(#dropdown{}) -> #dropdown.value;
value_field(#range{}) -> #range.value;
value_field(#icon{}) -> #icon.icon;
value_field(#image{}) -> #image.image;
value_field(Rec) ->
    Mod = element(#elementbase.module, Rec),
    Fields = Mod:reflect(),
    value_field(Fields, [value, text, body]).

value_field(Fields, [H|T]) ->
    case wf_utils:indexof(H, Fields) of
        undefined -> value_field(Fields, T);
        I -> I
    end;
value_field(_, []) ->
    undefined.

-define(def_val(EditID), {inplace_default_value, EditID}).
-define(edit_fun(EditID), {inplace_edit_fun, EditID}).
-define(view_fun(ViewID), {inplace_view_fun, ViewID}).

store_default_value(EditID, Value) ->
    wf:state(?def_val(EditID), Value).

get_default_value(EditID) ->
    wf:state_default(?def_val(EditID), undefined).

store_edit_fun(EditID, Fun) ->
    wf:state(?edit_fun(EditID), Fun).

get_edit_fun(EditID) ->
    wf:state_default(?edit_fun(EditID), undefined).

store_view_fun(ViewID, Fun) ->
    wf:state(?view_fun(ViewID), Fun).

get_view_fun(ViewID) ->
    wf:state_default(?view_fun(ViewID), undefined).


        
%is_element(R) when ?IS_ELEMENT(R) -> true;
%is_element(_) -> false.

-spec event(any()) -> ok.
event({ok, Delegate, Controls={ViewPanelID, _ViewID, EditPanelID, EditID}, Tag}) ->
	Module = wf:coalesce([Delegate, wf:page_module()]),
	Value = Module:inplace_event(Tag, string:strip(wf:q(EditID))),

    store_default_value(EditID, Value),
    redraw_view_and_edit(Controls, Value),

    ?WF_IF(not(?WF_BLANK(Value)), begin
        wf:wire(EditPanelID, #hide{}),
        wf:wire(ViewPanelID, #show{})
    end),
    ok;

event({cancel, Controls={ViewPanelID, _ViewID, EditPanelID, EditID}, _Tag}) ->
    DefValue = get_default_value(EditID),
    redraw_edit(Controls, DefValue),
    ?WF_IF(not(?WF_BLANK(DefValue)), begin
        wf:wire(EditPanelID, #hide{}),
        wf:wire(ViewPanelID, #show{})
    end),
    ok.

redraw_view_and_edit(Controls={ViewPanelID, ViewID, _EditPanelID, _EditID}, Value) ->
    ViewFun = get_view_fun(ViewID),
    wf:update(ViewPanelID, ViewFun(ViewID, Value)),
    redraw_edit(Controls, Value).

redraw_edit({_, _, EditPanelID, EditID}, Value) ->
    EditFun = get_edit_fun(EditID),
    wf:update(EditPanelID, EditFun(EditID, Value)).


maybe_modify_validators(Rec,  Trigger) ->
    Pred = fun
        (#validate{}) -> true;
        (_) -> false
    end,
    Update = fun
        (X = #validate{}) -> X#validate{trigger=Trigger};
        (X) -> X
    end,
    update_deep(Rec, Pred, Update).
