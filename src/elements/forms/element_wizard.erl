% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013 Rusty Klophaus/Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_wizard).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1,
    next_button_ids/2
]).
        
-spec reflect() -> [atom()].
reflect() -> record_info(fields, wizard).

-spec render_element(#wizard{}) -> body().
render_element(Record = #wizard{}) -> 
	% Set up callbacks...
	Tag = Record#wizard.tag,

	% Set up steps...
	wf:assert(Record#wizard.id /= undefined, wizard_needs_a_proper_name),
	wf:assert(is_list(Record#wizard.steps), wizard_steps_must_be_a_list),
	wf:assert(Record#wizard.titles == undefined orelse length(Record#wizard.titles) == length(Record#wizard.steps), wizard_incorrect_number_of_titles),
	StepCount = length(Record#wizard.steps),
	StepSeq = lists:seq(1, StepCount),
	StepIDs = [wf:to_atom("step" ++ integer_to_list(X)) || X <- StepSeq],
	
	% Function to create an individual step.
	F = fun(N) ->
		StepID = lists:nth(N, StepIDs),
		StepTitle = case Record#wizard.titles of
			undefined -> "";
			_ -> lists:nth(N, Record#wizard.titles)
		end,
		StepBody = lists:nth(N, Record#wizard.steps),
		IsFirst = (N == 1),
		IsLast = (N == StepCount),

		ButtonRow = fun(TopOrBot) ->
			#panel{class=[wizard_buttons,wf:to_list([TopOrBot,wizard_buttons])],body=[
				#singlerow{cells=[
					#tablecell{class=wizard_buttons_back,body=[
						#button{
							id=button_id(TopOrBot,back,N),
							show_if=(not IsFirst),
							text=Record#wizard.back,
							postback={back, N, StepIDs},
							delegate=?MODULE
						}
					]},
					#tablecell{class=wizard_buttons_next,body=[
						#button{
							id=button_id(TopOrBot,next,N), 
							show_if=(not IsLast), 
							text=Record#wizard.next, 
							postback={next, N, StepIDs}, 
							delegate=?MODULE 
						},
						#button{
							id=button_id(TopOrBot,finish,N), 
							show_if=IsLast, 
							text=Record#wizard.finish, 
							postback={finish, Tag}, 
							delegate=?MODULE 
						} 
					]}
				]}
			]}
		end,
		
		#panel { id=StepID, style="display: none;", body=[
			#panel{class=wizard_title,body=[
				#span{class=wizard_title_text,text=StepTitle},
                ?WF_IF(Record#wizard.show_progress, #span{
                    class=wizard_progress,
                    text=wf:f(Record#wizard.progress_text,[N, StepCount])
                })
			]},
			ButtonRow(top),
			#panel { class=wizard_body, body=StepBody },
			ButtonRow(bottom)
		]}
	end,

	% Combine the steps.
	Terms = #panel {
		class=[wizard,Record#wizard.class],
        data_fields=Record#wizard.data_fields,
		body=[F(X) || X <- StepSeq] 
	},
	
	% Show the first step.
	wf:wire(hd(StepIDs), #show{}),	
	
	% Render.
	Terms.
	
-spec event(term()) -> ok.
event({back, N, StepIDs}) -> 
	wf:wire(lists:nth(N, StepIDs), #hide {}),
	wf:wire(lists:nth(N - 1, StepIDs), #show {}),
	ok;	

event({next, N, StepIDs}) -> 
	wf:wire(lists:nth(N, StepIDs), #hide {}),
	wf:wire(lists:nth(N + 1, StepIDs), #show {}),
	ok;	

event({finish, Tag}) -> 
	Delegate = wf:page_module(),
	Delegate:wizard_event(Tag),
	ok.

-spec next_button_ids(Step :: integer(), NumSteps :: integer()) -> [id()].
next_button_ids(Step, NumSteps) ->
    NextOrFinish = ?WF_IF(Step==NumSteps,finish,next),
    [
        button_id(top, NextOrFinish, Step),
        button_id(bottom, NextOrFinish, Step)
    ].

-spec button_id(TB :: top|bottom, BNF :: back|next|finish, Step :: integer()) -> id().
button_id(TB, BNF, Step) ->
    wf:to_atom(wf:to_list([TB,"_",BNF,"_",Step])).

