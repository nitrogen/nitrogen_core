% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render_actions).
-include("wf.hrl").
-export ([
	render_action_queue/0,
    render_actions/2,
    render_actions/4,
    normalize_path/1,
    to_js_id/1,
    generate_anchor_script/2
]).

%%% RENDER ACTIONS %%%

render_action_queue() ->
	{ok, get_and_render_actions()}.
	
get_and_render_actions() ->
    case wf_context:next_action() of
		empty ->
			[];
		{ok, Action} ->
			{ok, JS} = render_actions(Action, undefined),
			[JS | get_and_render_actions()]
	end.

-spec render_actions(Actions :: actions(),
                     Anchor :: id()) -> {ok, script()}.
render_actions(Actions, Anchor) ->
    render_actions(Actions, Anchor, Anchor, Anchor).

-spec render_actions(Actions :: actions(),
                     Anchor :: id(),
                     Trigger :: id(),
                     Target :: id()) -> {ok, script()}.
render_actions(Actions, Anchor, Trigger, Target) ->
    Script = inner_render_actions(Actions, Anchor, Trigger, Target),
    {ok, Script}.

% @doc note: inner_render_actions always returns a list of of binaries or strings.
% This may not be necessary. But for now, we'll keep it like this.
-spec inner_render_actions(Actions :: actions(),
                           Anchor :: id(),
                           Trigger :: id(),
                           Target :: id()) -> script().
inner_render_actions(Actions, Anchor, Trigger, Target) ->
    if 
        Actions == [];
        Actions==undefined -> 
            [];
        is_binary(Actions) orelse ?IS_STRING(Actions)   -> 
            [Actions];
        is_tuple(Actions) ->    
            Script = inner_render_action(Actions, Anchor, Trigger, Target),
            [Script];
        is_list(Actions) ->
            [inner_render_actions(hd(Actions), Anchor, Trigger, Target)|
                inner_render_actions(tl(Actions), Anchor, Trigger, Target)];
        true ->
            throw({unanticipated_case_in_render_actions, Actions})
    end.

-spec inner_render_action(Action :: action_element(),
                          Anchor :: id(),
                          Trigger :: id(),
                          Target :: id()) -> script().
inner_render_action(Action, Anchor, Trigger, Target) when is_tuple(Action) ->
    Base = wf_utils:get_actionbase(Action),
    Module = Base#actionbase.module, 

    % Verify that this is an action...
    case Base#actionbase.is_action == is_action of
        true -> ok;
        false -> throw({not_an_action, Action})
    end,

    % Render...
    case Base#actionbase.show_if of 
        true -> 
            % Figure out the anchor, trigger, and target...
            Anchor1  = wf:coalesce([Base#actionbase.anchor, Anchor]),
            Anchor2  = normalize_path(Anchor1),
            Trigger1 = wf:coalesce([Base#actionbase.trigger, Trigger, Anchor]),
            Trigger2 = normalize_path(Trigger1),
            Target1  = wf:coalesce([Base#actionbase.target, Target, Anchor]),
            Target2  = normalize_path(Target1),

            Base1 = Base#actionbase {
                anchor = Anchor2,
                trigger = Trigger2,
                target = Target2
            },
            Action1 = wf_utils:replace_with_base(Base1, Action),

            % Render the action...
            ActionScript = call_action_render(Module, Action1, Anchor2, Trigger2, Target2),
            AnchorScript = generate_anchor_script_if_needed(ActionScript, Anchor2, Target2),
            case ActionScript /= undefined andalso not(wf_utils:is_iolist_empty(ActionScript)) of
                true  ->
                    DepJS = Base1#actionbase.dependency_js,
                    wrap_in_dependency(DepJS,[AnchorScript, ActionScript]);
                false -> []
            end;
        _ -> 
            []
    end.

-spec wrap_in_dependency(Url :: undefined | url(),
                         Script :: script()) -> script().
wrap_in_dependency(undefined, Script) ->
    Script;
wrap_in_dependency("", Script) ->
    Script;
wrap_in_dependency(Url, Script) ->
    [<<"Nitrogen.$dependency_register_function('">>,Url,<<"',function() {">>, Script, <<"});">>].

-spec generate_anchor_script_if_needed(ActionScript :: script(),
                                       Anchor :: id(),
                                       Target :: id()) -> script().
generate_anchor_script_if_needed(ActionScript, Anchor, Target) ->
	case needs_anchor_script(ActionScript) of
		true  -> generate_anchor_script(Anchor, Target);
		false -> ""
	end.

-spec needs_anchor_script(Script :: script()) -> boolean().
% @doc here we do a rudimentary check if a string starts with
% "Nitrogen.$anchor" discarding leading newlines and looking at the first depth
% where we encounter an element that isn't just another nested list. This
% should be faster than converting the whole script to a binary first just for
% this comparison.  That said, it's probably worthwhile to convert to binary
% and then doing all actions on it based on that. Most actions probably aren't
% that longm, so it shouldn't be that big of a performance hit.
needs_anchor_script(<<"Nitrogen.$anchor",_/binary>>) ->
	false;
needs_anchor_script("Nitrogen.$anchor" ++ _) ->
	false;
needs_anchor_script(<<"\n",Script/binary>>) ->
	needs_anchor_script(Script);
needs_anchor_script("\n" ++ Script) ->
	needs_anchor_script(Script);
needs_anchor_script([Script|_]) when is_list(Script); is_binary(Script) ->
    needs_anchor_script(Script);
needs_anchor_script(_) ->
	true.

-spec generate_anchor_script(Anchor :: id(), Target :: id()) -> script().
generate_anchor_script(Anchor, Target) ->
	wf:f(<<"\nNitrogen.$anchor('~s', '~s');">>, [Anchor, Target]).

% Calls the render_action/4 function of an action to turn an action record into Javascript.
-spec call_action_render(Module :: module(),
                         Action :: action_element(),
                         Anchor :: id(),
                         Trigger:: id(),
                         Target :: id()) -> script().
call_action_render(Module, Action, Anchor, Trigger, Target) ->
    case wf_utils:ensure_loaded(Module) of
        {module, Module} -> ok;
        {error, Error} -> throw({error_loading_action_module, Module, {error, Error}})
    end,
    NewActions = Module:render_action(Action),
    inner_render_actions(NewActions, Anchor, Trigger, Target).

% Turn an atom into ".wfid_atom"
% Turn atom.atom... into ".wfid_atom .wfid_atom"
% If it's a string, replace double "##" with ".wfid_"
-spec normalize_path(Path :: atom() | string() | binary()) -> string() | atom().
normalize_path(undefined) -> 
    undefined;
normalize_path(page) ->
    "page";
normalize_path(Path) when is_atom(Path) ->
    String = atom_to_list(Path),
    Tokens = string:tokens(String, "."),
    Tokens1 = [".wfid_" ++ X || X <- Tokens],
    string:join(Tokens1, " ");
normalize_path(String) when is_binary(String) ->
    normalize_path(binary_to_list(String));
normalize_path(String) ->
    case String of
        "wfid_" ++ _ -> "." ++ String;
        "temp" ++ _ -> ".wfid_" ++ String;
        _ -> wf_utils:replace(String, "##", ".wfid_")
    end.

-spec to_js_id(P :: [string()]) -> string().
to_js_id(P) ->
    P1 = lists:reverse(P),
    string:join(P1, ".").
