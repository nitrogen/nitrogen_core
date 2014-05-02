% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_lightbox).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, lightbox).

-spec render_element(#lightbox{}) -> body().
render_element(Record) -> 
    Panel = #panel {
        html_id=Record#lightbox.html_id,
        id=Record#lightbox.id,
        anchor=Record#lightbox.anchor,
        class=[lightbox, Record#lightbox.class],
        style=wf:to_list(Record#lightbox.style),
        title=Record#lightbox.title,
        data_fields=Record#lightbox.data_fields,
        body=[
            #panel { 			
                class=lightbox_background
            },
            #table { 
                class=lightbox_table,
                rows=#tablerow {
                    cells=#tablecell { align=center, valign=middle, style="vertical-align: middle;", body=[
                        "<center><table><tr><td>",
                        Record#lightbox.body,
                        "</td></tr></table></center>"
                    ]} 
                }
            }
        ]
    },
    element_panel:render_element(Panel).
