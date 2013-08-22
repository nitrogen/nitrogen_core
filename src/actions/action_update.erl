% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_update).
-include_lib ("wf.hrl").

-export([
	render_action/1,
	update/2,
	update/3,
	replace/2,
	replace/3,
	insert_top/2,
	insert_top/3,
	insert_bottom/2,
	insert_bottom/3,
	insert_before/2,
	insert_before/3,
	insert_after/2,
	insert_after/3,
	remove/1,
	remove/2
	]).


% This action is used internally by Nitrogen.
render_action(Record) ->
    Type    = Record#update.type,
    Anchor  = Record#update.anchor,
    %Trigger = Record#update.trigger,
    Target  = Record#update.target,

	%% If there are any actions wired directly to the elements below, the Anchor, Trigger, and Target
	%% above should be passed.
	%%
	%% For example: Something like wf:replace(#panel{text="whatever",actions=#hide{}}).
	%%
	%% But we can test without it for now

    % Render into HTML and Javascript...
    Elements = Record#update.elements,
	{ok, Html} = wf_render_elements:render_elements(Elements),

    % Turn the HTML into a Javascript statement that will update the right element.
	ScriptifiedHtml = wf:f(<<"Nitrogen.$~s(\"~s\", \"~s\", \"~s\");">>, [Type, Anchor, Target, wf:js_escape(Html)]),

	ScriptifiedHtml.


update(Target, Elements) -> 
    update(normal, Target, Elements).

update(Priority, Target, Elements) ->
	update(Priority, update, Target, Elements).

replace(Target, Elements) ->
    replace(normal, Target, Elements).

replace(Priority, Target, Elements) ->
    update(Priority, replace, Target, Elements).

insert_top(Target, Elements) -> 
    insert_top(normal, Target, Elements).

insert_top(Priority, Target, Elements) -> 
    update(Priority, insert_top, Target, Elements).

insert_bottom(Target, Elements) -> 
    insert_bottom(normal, Target, Elements).

insert_bottom(Priority, Target, Elements) -> 
    update(Priority, insert_bottom, Target, Elements).

insert_before(Target, Elements) ->
    insert_before(normal, Target, Elements).

insert_before(Priority, Target, Elements) ->
    update(Priority, insert_before, Target, Elements).

insert_after(Target, Elements) ->
    insert_after(normal, Target, Elements).

insert_after(Priority, Target, Elements) ->
    update(Priority, insert_after, Target, Elements).

remove(Target) ->
	remove(normal, Target).

remove(Priority, Target) ->
	update(Priority, remove, Target, []).

%%% PRIVATE FUNCTIONS %%%

update(Priority, Type, Target, Elements) ->
    Anchor = wf_context:anchor(),
    Action = #update {
        type=Type,
        anchor  = Anchor, 
        target  = wf:coalesce([Target, Anchor]), 
        elements=Elements		
    },

    wf_context:add_action(Priority, [Action]),
    ok.
