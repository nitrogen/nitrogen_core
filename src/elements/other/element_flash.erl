% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_flash).
-include_lib ("wf.hrl").
-export([
	reflect/0,
	render_element/1,
	render/0,
	add_flash/1,
	add_flash/2,
	render_flash/2
]).

reflect() -> record_info(fields, flash).

render_element(_Record) -> 
    Terms = #panel { 
        id=flash,
        class=flash_container
    },
    wf:state(has_flash, true),
    Terms.

% render/0 - Convenience methods to place the flash element on a page from a template.
render() -> #flash{}.

add_flash(Term) ->
    FlashID = wf:temp_id(),
    add_flash(FlashID, Term).

add_flash(FlashID, Term) ->
	Elements = render_flash(FlashID, Term),
	wf:insert_bottom(defer, flash, Elements).

render_flash(FlashID, Term) ->
	#panel{
		id=FlashID,
		style="display: none;",
		body=[
			#panel{
				class=flash,
				actions=#show{target=FlashID, effect=blind, speed=400},
				body=[
					#link{
						class=flash_close_button,
						text="Close",
						actions=
							#event{type=click, target=FlashID, actions=
								#hide{effect=blind, speed=400}
							}
					},
					#panel{class=flash_content, body=Term }
				]
			}
		]
	}.
