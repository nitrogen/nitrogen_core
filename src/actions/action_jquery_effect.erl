% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_jquery_effect).
-include("wf.hrl").
-export([
    render_action/1,
    options_to_js/1
]).

-spec render_action(#jquery_effect{}) -> body().
render_action(Record) ->
    Target = Record#jquery_effect.target,
    Effect = Record#jquery_effect.effect,
    Speed = Record#jquery_effect.speed, 
    Options = options_to_js(Record#jquery_effect.options),
    Class = Record#jquery_effect.class,
    Easing = Record#jquery_effect.easing,
    {ActionsFun, Actions} = case Record#jquery_effect.actions of
        undefined ->
            {"null", ""};
        Actions1 ->
            {["function() {", Actions1, "}"], ""}
    end,

    %% For the show or hide type effects, we want the followup action triggers
    %% to happen even if the element doesn't have to animate.  For example, if
    %% we choose to "slideUp", we want to to do the followup actions even if the
    %% element is already hidden.
    %% This "is_visible" function does this logic for us.

    Script = case Record#jquery_effect.type of
        'show' when Effect==none ->
            {raw, ["this.forEach(node => { node.classList.remove('n2-effect-hidden'); } )"]};
        'hide' when Effect==none ->
            {raw, ["this.forEach(node => { node.classList.remove('n2-effect', 'n2-effect-fase', 'n2-effect-appear'); node.classList.add('n2-effect-hidden'); } )"]};
        'toggle' when Effect==none ->
            {raw, ["this.forEach(node => { node.classList.remove('n2-effect', 'n2-effect-fase', 'n2-effect-appear'); node.classList.toggle('n2-effect-hidden'); } )"]};
        'appear' ->
            {raw, ["this.forEach(node => { node.classList.remove('n2-effect-fade'); node.classList.add('n2-effect', 'n2-effect-appear'); } )"]};
        'fade'   ->
            {raw, ["this..forEach(node => { node.classList.remove('n2-effect-appear'); node.classList.add('n2-effect', 'n2-effect-fade'); } )"]};
        'slideup' ->
            if_visible([wf:f("$(this).slideUp(~p, ",[Speed]), ActionsFun, ");"], Actions);
        'slidedown' ->
            if_visible(Actions, [wf:f("$(this).slideDown(~p, ",[Speed]), ActionsFun, ");"]);
        'show' ->
            if_visible(Actions, [wf:f("$(this).show('~s', ~s, ~p, ", [Effect, Options, Speed]), ActionsFun, ");"]);
        'hide' ->
            if_visible([wf:f("$(this).hide('~s', ~s, ~p, ", [Effect, Options, Speed]), ActionsFun, ");"], Actions);
        'effect' ->
            [wf:f("effect('~s', ~s, ~p, ", [Effect, Options, Speed]), ActionsFun, ");"];
        'toggle' ->
            [wf:f("toggle('~s', ~s, ~p, ", [Effect, Options, Speed]), ActionsFun, ");"];
        'add_class' when Speed==undefined; Speed==0 ->
            [wf:f("addClass('~s'); ", [Class]), execute_actions_fun(ActionsFun), ";"];
        'add_class' ->
            [wf:f("addClass('~s', ~p, ", [Class, Speed]), ActionsFun, ");"];
        'remove_class' when Speed==undefined; Speed==0 ->
            [wf:f("removeClass('~s'); ",[Class]), execute_actions_fun(ActionsFun), ";"];
        'remove_class' ->
            [wf:f("removeClass('~s', ~p, ", [Class, Speed]), ActionsFun, ");"];
        'animate' ->
            [wf:f("animate(~s, ~p, '~s', ", [Options, Speed, Easing]), ActionsFun, ");"]
    end,
    R = case Script of
        {raw, S} -> [";(function(){", S, "}.bind(", wf:f("document.querySelectorAll('~s')", [Target]), ")})();"];
        S        -> [wf:f("objs('~s').", [Target]), Script]
    end,
    io:format("Effect: ~p~n", [R]),
    R.

execute_actions_fun("null") ->
    "";
execute_actions_fun(ActionsFun) ->
    [ActionsFun,"()"].

if_visible(IfVisible, IfInvisible) ->
    [
        "each(function(){
            if($(this).is(':visible')) {",
                IfVisible,
            "} else {",
                IfInvisible,
            "}
        });"
    ].


-spec options_to_js(Options :: proplist()) -> binary().
options_to_js(Options) ->
    F = fun({Key, Value}) ->
        if 
            Value =:= true; Value =:= false ->
                wf:f(<<"~s: ~ts">>, [Key, Value]);
            is_list(Value); is_binary(Value); is_atom(Value) ->
                wf:f(<<"~s: '~ts'">>, [Key, wf:js_escape(Value)]);
            true -> 
                wf:f(<<"~s: ~p">>, [Key, Value])
        end
    end,
    Options1 = [F(X) || X <- Options],
    Options2 = wf:join(Options1, <<",">>),
    wf:f(<<"{ ~s }">>, [Options2]).




