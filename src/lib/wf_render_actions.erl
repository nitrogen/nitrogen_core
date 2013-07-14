% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render_actions).
-include_lib ("wf.hrl").
-export ([
	render_action_queue/1,
    render_actions/2,
    render_actions/4,
    normalize_path/1,
    to_js_id/1,
    generate_anchor_script/2
]).

%%% RENDER ACTIONS %%%

render_action_queue(ActionQueue) ->
	{ok, get_and_render_actions(ActionQueue)}.
	
get_and_render_actions(ActionQueue) ->
	case wf_action_queue:out(ActionQueue) of
		{error, empty} ->
			[];
		{ok, Action} ->
			{ok, JS} = wf_render_actions:render_actions(Action, undefined),
			[JS | get_and_render_actions(ActionQueue)]
	end.

% render_actions(Actions) -> {ok, Script}.
render_actions(Actions, Anchor) ->
    render_actions(Actions, Anchor, Anchor, Anchor).

render_actions(Actions, Anchor, Trigger, Target) ->
    Script = inner_render_actions(Actions, Anchor, Trigger, Target),
    {ok, Script}.

% render_actions(Actions, ScriptAcc) -> {ok, Script}.
inner_render_actions(Action, Anchor, Trigger, Target) ->
    if 
        Action == [] orelse Action==undefined -> 
            [];
        is_binary(Action) orelse ?IS_STRING(Action)   -> 
            [Action];
        is_tuple(Action) ->    
            Script = inner_render_action(Action, Anchor, Trigger, Target),
            [Script];
        is_list(Action) ->
            [inner_render_actions(hd(Action), Anchor, Trigger, Target)|
                inner_render_actions(tl(Action), Anchor, Trigger, Target)];
        true ->
            throw({unanticipated_case_in_render_actions, Action})
    end.

% render_action(Action) -> {ok, Script}.
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

wrap_in_dependency(undefined, Script) ->
    Script;
wrap_in_dependency("", Script) ->
    Script;
wrap_in_dependency(Url, Script) ->
    [<<"Nitrogen.$dependency_register_function('">>,Url,<<"',function() {">>, Script, <<"});">>].

generate_anchor_script_if_needed(ActionScript, Anchor, Target) ->
	case needs_anchor_script(ActionScript) of
		true  -> generate_anchor_script(Anchor, Target);
		false -> ""
	end.

needs_anchor_script(undefined) -> 
    false;
needs_anchor_script(<<"Nitrogen.$anchor",_/binary>>) ->
	false;
needs_anchor_script("Nitrogen.$anchor" ++ _) ->
	false;
needs_anchor_script(<<"\n",Script/binary>>) ->
	needs_anchor_script(Script);
needs_anchor_script("\n" ++ Script) ->
	needs_anchor_script(Script);
needs_anchor_script(_) ->
	true.


generate_anchor_script(Anchor, Target) ->
	wf:f(<<"\nNitrogen.$anchor('~s', '~s');">>, [Anchor, Target]).

% call_action_render(Module, Action) -> {ok, Script}.
% Calls the render_action/4 function of an action to turn an action record into Javascript.
call_action_render(Module, Action, Anchor, Trigger, Target) ->
    {module, Module} = code:ensure_loaded(Module),
    NewActions = Module:render_action(Action),
    inner_render_actions(NewActions, Anchor, Trigger, Target).

% Turn an atom into ".wfid_atom"
% Turn atom.atom... into ".wfid_atom .wfid_atom"
% If it's a string, replace double "##" with ".wfid_"
normalize_path(undefined) -> 
    undefined;
normalize_path(page) ->
    "page";
normalize_path(Path) when is_atom(Path) ->
    String = atom_to_list(Path),
    Tokens = string:tokens(String, "."),
    Tokens1 = [".wfid_" ++ X || X <- Tokens],
    string:join(Tokens1, " ");
normalize_path(String) ->
    case String of
        "wfid_" ++ _ -> "." ++ String;
        "temp" ++ _ -> ".wfid_" ++ String;
        _ -> wf_utils:replace(String, "##", ".wfid_")
    end.

to_js_id(P) ->
    P1 = lists:reverse(P),
    string:join(P1, ".").
